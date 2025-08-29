"""
utils.py - Utility functions for Jarvis
Place this file in the same directory as enhanced_jarvis.py
This file contains helper functions and utilities
"""

import os
import json
import logging
import datetime
import subprocess
import platform
import requests
import psutil
from pathlib import Path
from typing import Dict, List, Optional, Tuple

# Set up logging
def setup_logging(log_file: Path, level: str = 'INFO'):
    """Set up logging configuration"""
    logging.basicConfig(
        level=getattr(logging, level),
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler(log_file),
            logging.StreamHandler()
        ]
    )
    return logging.getLogger('Jarvis')

class SystemUtils:
    """System-related utility functions"""
    
    @staticmethod
    def get_system_info() -> Dict:
        """Get comprehensive system information"""
        return {
            'platform': platform.system(),
            'platform_release': platform.release(),
            'platform_version': platform.version(),
            'architecture': platform.machine(),
            'hostname': platform.node(),
            'processor': platform.processor(),
            'cpu_count': psutil.cpu_count(),
            'memory_total': psutil.virtual_memory().total,
            'python_version': platform.python_version()
        }
    
    @staticmethod
    def get_process_list() -> List[Dict]:
        """Get list of running processes"""
        processes = []
        for proc in psutil.process_iter(['pid', 'name', 'cpu_percent', 'memory_percent']):
            try:
                processes.append(proc.info)
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        return processes
    
    @staticmethod
    def kill_process(process_name: str) -> bool:
        """Kill a process by name"""
        for proc in psutil.process_iter(['name']):
            try:
                if process_name.lower() in proc.info['name'].lower():
                    proc.terminate()
                    return True
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        return False
    
    @staticmethod
    def get_network_info() -> Dict:
        """Get network information"""
        info = {}
        try:
            # Get network interfaces
            interfaces = psutil.net_if_addrs()
            info['interfaces'] = list(interfaces.keys())
            
            # Get network stats
            stats = psutil.net_if_stats()
            info['active_interfaces'] = [
                iface for iface, stat in stats.items() if stat.isup
            ]
            
            # Try to get external IP
            try:
                response = requests.get('https://api.ipify.org?format=json', timeout=2)
                info['external_ip'] = response.json()['ip']
            except:
                info['external_ip'] = 'Unable to determine'
                
        except Exception as e:
            info['error'] = str(e)
            
        return info

class FileUtils:
    """File and directory utility functions"""
    
    @staticmethod
    def search_files(directory: Path, pattern: str, recursive: bool = True) -> List[Path]:
        """Search for files matching a pattern"""
        if recursive:
            return list(directory.rglob(pattern))
        else:
            return list(directory.glob(pattern))
    
    @staticmethod
    def get_recent_files(directory: Path, count: int = 10) -> List[Tuple[Path, datetime.datetime]]:
        """Get most recently modified files in a directory"""
        files = []
        for file_path in directory.iterdir():
            if file_path.is_file():
                mtime = datetime.datetime.fromtimestamp(file_path.stat().st_mtime)
                files.append((file_path, mtime))
        
        files.sort(key=lambda x: x[1], reverse=True)
        return files[:count]
    
    @staticmethod
    def get_directory_size(directory: Path) -> int:
        """Get total size of a directory in bytes"""
        total_size = 0
        for dirpath, dirnames, filenames in os.walk(directory):
            for filename in filenames:
                filepath = os.path.join(dirpath, filename)
                try:
                    total_size += os.path.getsize(filepath)
                except:
                    pass
        return total_size
    
    @staticmethod
    def format_bytes(bytes_size: int) -> str:
        """Format bytes to human readable string"""
        for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
            if bytes_size < 1024.0:
                return f"{bytes_size:.2f} {unit}"
            bytes_size /= 1024.0
        return f"{bytes_size:.2f} PB"

class CommandHistory:
    """Manage command history"""
    
    def __init__(self, history_file: Path, max_size: int = 100):
        self.history_file = history_file
        self.max_size = max_size
        self.history = self.load_history()
    
    def load_history(self) -> List[Dict]:
        """Load command history from file"""
        if self.history_file.exists():
            try:
                with open(self.history_file, 'r') as f:
                    return json.load(f)
            except:
                return []
        return []
    
    def save_history(self):
        """Save command history to file"""
        try:
            with open(self.history_file, 'w') as f:
                json.dump(self.history[-self.max_size:], f, indent=2, default=str)
        except Exception as e:
            print(f"Error saving history: {e}")
    
    def add_command(self, command: str, result: str = None):
        """Add a command to history"""
        entry = {
            'timestamp': datetime.datetime.now().isoformat(),
            'command': command,
            'result': result
        }
        self.history.append(entry)
        
        # Trim history if it exceeds max size
        if len(self.history) > self.max_size:
            self.history = self.history[-self.max_size:]
        
        self.save_history()
    
    def get_recent_commands(self, count: int = 10) -> List[Dict]:
        """Get recent commands"""
        return self.history[-count:]
    
    def search_history(self, query: str) -> List[Dict]:
        """Search command history"""
        results = []
        for entry in self.history:
            if query.lower() in entry['command'].lower():
                results.append(entry)
        return results

class WebUtils:
    """Web-related utility functions"""
    
    @staticmethod
    def get_weather(city: str, api_key: str) -> Optional[Dict]:
        """Get weather information from OpenWeatherMap"""
        if not api_key:
            return None
            
        try:
            url = f"http://api.openweathermap.org/data/2.5/weather?q={city}&appid={api_key}&units=metric"
            response = requests.get(url, timeout=5)
            
            if response.status_code == 200:
                data = response.json()
                return {
                    'temperature': data['main']['temp'],
                    'feels_like': data['main']['feels_like'],
                    'description': data['weather'][0]['description'],
                    'humidity': data['main']['humidity'],
                    'wind_speed': data['wind']['speed']
                }
        except Exception as e:
            print(f"Weather API error: {e}")
            
        return None
    
    @staticmethod
    def get_news(api_key: str, country: str = 'us') -> Optional[List[Dict]]:
        """Get news headlines from NewsAPI"""
        if not api_key:
            return None
            
        try:
            url = f"https://newsapi.org/v2/top-headlines?country={country}&apiKey={api_key}"
            response = requests.get(url, timeout=5)
            
            if response.status_code == 200:
                data = response.json()
                return data.get('articles', [])[:5]  # Return top 5 articles
        except Exception as e:
            print(f"News API error: {e}")
            
        return None
    
    @staticmethod
    def search_wikipedia(query: str) -> Optional[str]:
        """Search Wikipedia and get summary"""
        try:
            import wikipedia
            summary = wikipedia.summary(query, sentences=2)
            return summary
        except ImportError:
            print("Wikipedia module not installed. Install with: pip install wikipedia-api")
        except Exception as e:
            print(f"Wikipedia search error: {e}")
            
        return None

class AudioUtils:
    """Audio-related utility functions"""
    
    @staticmethod
    def list_audio_devices():
        """List available audio devices"""
        import sounddevice as sd
        return sd.query_devices()
    
    @staticmethod
    def test_microphone(duration: int = 3) -> bool:
        """Test if microphone is working"""
        try:
            import sounddevice as sd
            import numpy as np
            
            recording = sd.rec(int(duration * 44100), samplerate=44100, channels=1)
            sd.wait()
            
            # Check if we got any sound
            max_amplitude = np.max(np.abs(recording))
            if max_amplitude > 0.01:  # Threshold for detecting sound
                return True
            else:
                return False
                
        except Exception as e:
            return False
    
    @staticmethod
    def play_beep(frequency: int = 440, duration: float = 0.2):
        """Play a beep sound"""
        try:
            import sounddevice as sd
            import numpy as np
            
            sample_rate = 44100
            t = np.linspace(0, duration, int(sample_rate * duration))
            waveform = np.sin(frequency * 2 * np.pi * t)
            
            sd.play(waveform, sample_rate)
            sd.wait()
        except Exception as e:
            pass

class TextProcessor:
    """Text processing utilities"""
    
    @staticmethod
    def extract_numbers(text: str) -> List[float]:
        """Extract numbers from text"""
        import re
        numbers = re.findall(r'\d+\.?\d*', text)
        return [float(n) for n in numbers]
    
    @staticmethod
    def extract_time(text: str) -> Optional[datetime.time]:
        """Extract time from text"""
        import re
        
        # Pattern for time formats (e.g., 3:30 PM, 15:30)
        patterns = [
            r'(\d{1,2}):(\d{2})\s*(AM|PM|am|pm)?',
            r'(\d{1,2})\s*(AM|PM|am|pm)'
        ]
        
        for pattern in patterns:
            match = re.search(pattern, text)
            if match:
                groups = match.groups()
                hour = int(groups[0])
                minute = int(groups[1]) if len(groups) > 1 and groups[1] else 0
                
                if len(groups) > 2 and groups[2]:
                    # 12-hour format
                    period = groups[2].upper()
                    if period == 'PM' and hour != 12:
                        hour += 12
                    elif period == 'AM' and hour == 12:
                        hour = 0
                
                return datetime.time(hour, minute)
        
        return None
    
    @staticmethod
    def fuzzy_match(query: str, options: List[str], threshold: float = 0.6) -> Optional[str]:
        """Find best fuzzy match for a query in a list of options"""
        try:
            from difflib import SequenceMatcher
            
            best_match = None
            best_score = 0
            
            for option in options:
                score = SequenceMatcher(None, query.lower(), option.lower()).ratio()
                if score > best_score and score >= threshold:
                    best_score = score
                    best_match = option
            
            return best_match
        except Exception as e:
            print(f"Fuzzy matching error: {e}")
            return None

# Test utilities function removed - no debug prints in production