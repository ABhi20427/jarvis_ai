from typing import Dict
import sounddevice as sd
import soundfile as sf
import speech_recognition as sr
import pyttsx3
import time
import os
import webbrowser
import datetime
import subprocess
import platform
import json
import threading
import queue
import random
import psutil
import requests
from pathlib import Path
import pygetwindow as gw
import pyautogui
import signal
import sys

# AI imports
from ai_brain_local import JarvisLocalAI
from config import AIConfig


# Initialize TTS engine with better error handling
class TTSManager:
    """Thread-safe TTS Manager to prevent run loop conflicts"""
    
    def __init__(self):
        self.is_speaking = False
        self.speech_lock = threading.Lock()
        self._engine = None
        self._initialize_engine()
    
    def _initialize_engine(self):
        """Initialize TTS engine once"""
        try:
            self._engine = pyttsx3.init()
            self._engine.setProperty('rate', 180)
            self._engine.setProperty('volume', 0.9)
            print("✅ TTS Manager initialized successfully")
        except Exception as e:
            print(f"❌ TTS Manager initialization failed: {e}")
            self._engine = None
    
    def speak_text(self, text, rate=180, volume=0.9, timeout=10):
        """Speak text with thread safety and timeout"""
        if not self._engine:
            print(f"[TTS Not Available] Would have said: {text}")
            return False
            
        with self.speech_lock:
            try:
                if self.is_speaking:
                    return False  # Skip if already speaking
                
                self.is_speaking = True
                
                # Use the existing engine instead of creating new ones
                self._engine.setProperty('rate', rate)
                self._engine.setProperty('volume', volume)
                self._engine.say(text)
                
                
                # Use threading with timeout to prevent hanging
                import threading
                import time
                
                def run_tts():
                    try:
                        self._engine.runAndWait()
                    except Exception as e:
                        print(f"TTS thread error: {e}")
                
                tts_thread = threading.Thread(target=run_tts, daemon=True)
                tts_thread.start()
                tts_thread.join(timeout=timeout)
                
                if tts_thread.is_alive():
                    print(f"TTS timeout after {timeout} seconds, skipping speech")
                    self.is_speaking = False
                    return False
                
                self.is_speaking = False
                return True
                
            except Exception as e:
                self.is_speaking = False
                print(f"TTS Manager Error: {e}")
                # Try to reinitialize engine if it failed
                self._initialize_engine()
                return False
    
    def cleanup(self):
        """Clean up TTS engine"""
        try:
            if self._engine:
                self._engine.stop()
                del self._engine
                self._engine = None
        except:
            pass
class EnhancedJarvis:
    def __init__(self):
        # Audio settings
        self.recording_duration = 5  # Increased for longer commands
        self.sample_rate = 44100     # Higher quality audio
        self.channels = 1
        
        # System info
        self.system = platform.system()
        self.user_name = os.environ.get('USERNAME', 'User')
        
        # Recognition settings
        self.recognizer = sr.Recognizer()
        self.recognizer.energy_threshold = 4000  # Adjust based on environment
        self.recognizer.dynamic_energy_threshold = True
        
        # Command queue for async processing
        self.command_queue = queue.Queue()
        
        # Status flags
        self.is_running = True
        self.is_listening = False
        self.continuous_listening = False
        
        # Conversation context
        self.last_command = None
        self.command_history = []
        self.context = {}
        
        # Initialize TTS Manager FIRST (before AI brain)
        self.tts_manager = TTSManager()

        # Wake words
        self.wake_words = ["jarvis", "hey jarvis", "okay jarvis", "computer"]

        # Initialize responses
        self.init_responses()

        # Build command cache for faster processing
        self._build_command_cache()

        # Optimize performance
        self._optimize_performance()

        # Initialize AI Brain (AFTER TTS Manager)
        self.init_ai_brain()

        print("🤖 Enhanced Jarvis System Initializing...")
        # Add small delay to ensure TTS is ready
        time.sleep(0.5)
        self.startup_sequence()

    def _optimize_performance(self):
        """Optimize system performance for voice processing"""
        try:
            import threading
            import gc
            
            # Set thread priorities for audio processing
            audio_thread = threading.current_thread()
            if hasattr(audio_thread, 'set_priority'):
                audio_thread.set_priority('high')
            
            # Optimize garbage collection
            gc.set_threshold(700, 10, 10)  # More aggressive GC
            
            # Pre-import heavy modules
            self._preload_modules()
            
        except Exception as e:
            print(f"⚠️ Performance optimization failed: {e}")

    def _preload_modules(self):
        """Preload commonly used modules for faster access"""
        try:
            import webbrowser
            import datetime
            import subprocess
            import psutil
            # Preload in background thread to avoid blocking
            threading.Thread(target=self._background_preload, daemon=True).start()
        except Exception as e:
            print(f"⚠️ Module preloading failed: {e}")

    def _background_preload(self):
        """Background preloading of heavy modules"""
        try:
            import numpy as np
            import scipy.signal
            import difflib
        except ImportError:
            pass  # Optional modules

    def init_ai_brain(self):
        """Initialize the AI brain"""
        if AIConfig.ENABLE_AI:
            try:
                self.ai = JarvisLocalAI(model=AIConfig.AI_MODEL)
                if self.ai.is_available:
                    self.ai_enabled = True
                    print("🧠 AI Brain initialized successfully")
                    # Only speak if TTS manager is available
                    if hasattr(self, 'tts_manager'):
                        self.speak("AI capabilities online. I'm now smarter than ever.", wait=False)
                else:
                    self.ai_enabled = False
                    print("⚠️ AI Brain offline - Ollama not running")
            except Exception as e:
                print(f"❌ AI Brain initialization failed: {e}")
                self.ai_enabled = False
        else:
            self.ai_enabled = False

    def init_responses(self):
        """Initialize varied responses for better interaction"""
        self.responses = {
            'greeting': [
                f"Hello {self.user_name}, how can I assist you today?",
                f"Good to see you, {self.user_name}. What can I do for you?",
                f"At your service, {self.user_name}.",
                "Systems online. How may I help you?",
                "Ready for your commands, sir."
            ],
            'acknowledgment': [
                "Right away, sir.",
                "Understood.",
                "Processing your request.",
                "On it.",
                "Consider it done."
            ],
            'error': [
                "I encountered an issue. Would you like me to try again?",
                "There seems to be a problem. Let me investigate.",
                "I'm having trouble with that request.",
                "Something went wrong. Shall I attempt an alternative approach?"
            ],
            'listening': [
                "I'm listening...",
                "Yes?",
                "What can I do for you?",
                "Go ahead.",
                "I'm ready for your command."
            ]
        }

    def _build_command_cache(self):
        """Build a cache of commands for faster processing"""
        self.command_cache = {
            # System commands
            'shutdown': ['shutdown', 'shut down', 'power off'],
            'restart': ['restart', 'reboot'],
            'lock': ['lock', 'lock screen', 'lock computer'],
            'sleep': ['sleep', 'sleep mode', 'hibernate'],
            
            # Applications (most common first)
            'chrome': ['chrome', 'google chrome', 'browser'],
            'firefox': ['firefox', 'mozilla'],
            'vscode': ['vs code', 'visual studio code', 'vscode', 'code'],
            'notepad': ['notepad', 'text editor'],
            'calculator': ['calculator', 'calc'],
            'explorer': ['explorer', 'file explorer', 'files'],
            'cmd': ['command prompt', 'cmd', 'terminal'],
            
            # Web commands
            'youtube': ['youtube', 'you tube'],
            'google': ['google', 'search google'],
            'gmail': ['gmail', 'email'],
            
            # System info
            'time': ['time', 'what time', 'current time'],
            'date': ['date', 'today', 'what date'],
            'battery': ['battery', 'power status'],
            'cpu': ['cpu', 'processor', 'cpu usage'],
            'memory': ['memory', 'ram', 'memory usage'],
            
            # Volume
            'volume_up': ['volume up', 'increase volume', 'louder'],
            'volume_down': ['volume down', 'decrease volume', 'quieter'],
            'mute': ['mute', 'silence'],
            'unmute': ['unmute', 'sound on'],
            
            # File operations
            'screenshot': ['screenshot', 'screen shot', 'capture screen'],
            'documents': ['documents', 'documents folder'],
            'downloads': ['downloads', 'downloads folder'],
            'desktop': ['desktop', 'desktop folder']
        }
        
        # Build reverse lookup for faster matching
        self.keyword_to_command = {}
        for command, keywords in self.command_cache.items():
            for keyword in keywords:
                self.keyword_to_command[keyword] = command

    def _fast_command_match(self, command):
        """Fast command matching using cache"""
        # Direct keyword match
        if command in self.keyword_to_command:
            return self.keyword_to_command[command]
        
        # Partial match
        for keyword, mapped_command in self.keyword_to_command.items():
            if keyword in command:
                return mapped_command
        
        # Fuzzy match for close matches
        return self._fuzzy_command_match(command)

    def _fuzzy_command_match(self, command):
        """Fuzzy matching for similar commands"""
        from difflib import SequenceMatcher
        
        best_match = None
        best_score = 0.7  # Threshold for fuzzy matching
        
        for keyword in self.keyword_to_command.keys():
            similarity = SequenceMatcher(None, command, keyword).ratio()
            if similarity > best_score:
                best_score = similarity
                best_match = self.keyword_to_command[keyword]
        
        return best_match

    def startup_sequence(self):
        """Jarvis-like startup sequence with fixed TTS"""
        startup_messages = [
            "System initialization complete.",
            f"Welcome back, {self.user_name}.",
            f"All systems operational. Current time is {datetime.datetime.now().strftime('%I:%M %p')}.",
            "Ready for your commands."
        ]
        
        for i, message in enumerate(startup_messages):
            print(f"🤖 {message}")
            # Use blocking speech for startup sequence for proper timing
            self.speak(message, wait=True)
            if i < len(startup_messages) - 1:  # Don't wait after last message
                time.sleep(0.8)

    def speak(self, text, wait=True, priority=False):
        print(f"🔊 Jarvis: {text}")
        
        def do_speak():
            success = self.tts_manager.speak_text(text)
            if not success:
                print(f"[TTS Failed] Would have said: {text}")
        
        if wait:
            do_speak()
        else:
            thread = threading.Thread(target=do_speak, daemon=True)
            thread.start()
            if priority:
                thread.join(timeout=3)

    def get_random_response(self, response_type):
        """Get varied responses for natural interaction"""
        return random.choice(self.responses.get(response_type, ["Processing..."]))

    def listen_for_audio(self, timeout=None, phrase_limit=None):
        """Enhanced listening with noise reduction and better audio processing"""
        try:
            with sr.Microphone(sample_rate=self.sample_rate) as source:
                # Dynamic noise adjustment
                print("🔧 Calibrating microphone...")
                self.recognizer.adjust_for_ambient_noise(source, duration=1.0)
                
                # Optimize recognition settings
                self.recognizer.energy_threshold = max(300, self.recognizer.energy_threshold)
                self.recognizer.pause_threshold = 0.8
                self.recognizer.phrase_threshold = 0.3
                self.recognizer.non_speaking_duration = 0.5
                
                print("🎤 Listening...")
                self.is_listening = True
                
                # Record with better error handling
                try:
                    audio = self.recognizer.listen(
                        source,
                        timeout=timeout or 5,
                        phrase_time_limit=phrase_limit or 10
                    )
                    
                    # Audio preprocessing
                    audio = self._preprocess_audio(audio)
                    
                    self.is_listening = False
                    return audio
                    
                except sr.WaitTimeoutError:
                    print("⏰ Listening timeout")
                    self.is_listening = False
                    return None
                    
        except Exception as e:
            print(f"❌ Microphone error: {e}")
            self.is_listening = False
            return None

    def _preprocess_audio(self, audio):
        """Preprocess audio for better recognition"""
        try:
            import numpy as np
            from scipy import signal
            
            # Convert to numpy array
            audio_data = np.frombuffer(audio.get_raw_data(), np.int16)
            
            # Apply noise reduction (simple high-pass filter)
            # Remove low-frequency noise
            nyquist = self.sample_rate // 2
            low_cutoff = 300 / nyquist
            b, a = signal.butter(4, low_cutoff, btype='high')
            filtered_audio = signal.filtfilt(b, a, audio_data)
            
            # Normalize audio levels
            filtered_audio = filtered_audio / np.max(np.abs(filtered_audio))
            filtered_audio = (filtered_audio * 32767).astype(np.int16)
            
            # Convert back to AudioData
            processed_audio = sr.AudioData(
                filtered_audio.tobytes(),
                audio.sample_rate,
                audio.sample_width
            )
            
            return processed_audio
            
        except ImportError:
            print("⚠️ scipy not available, using raw audio")
            return audio
        except Exception as e:
            print(f"⚠️ Audio preprocessing failed: {e}")
            return audio

    def recognize_speech(self, audio):
        """Enhanced speech recognition with multiple engines and preprocessing"""
        if audio is None:
            return None
        
        # Try multiple recognition engines in order of preference
        recognition_methods = [
            ('Google', self._recognize_google),
            ('Google Cloud', self._recognize_google_cloud),
            ('Sphinx', self._recognize_sphinx),
            ('Wit.ai', self._recognize_wit)
        ]
        
        for method_name, method in recognition_methods:
            try:
                text = method(audio)
                if text:
                    # Clean and normalize the text
                    text = self._clean_recognized_text(text)
                    print(f"✅ Recognition ({method_name}): {text}")
                    return text
            except Exception as e:
                print(f"⚠️ {method_name} recognition failed: {e}")
                continue
        
        print("❌ All recognition methods failed")
        return None

    def _recognize_google(self, audio):
        """Google Speech Recognition with enhanced settings"""
        return self.recognizer.recognize_google(
            audio, 
            language='en-US',
            show_all=False
        ).lower()

    def _recognize_google_cloud(self, audio):
        """Google Cloud Speech (if API key available)"""
        # Add your Google Cloud credentials if available
        return self.recognizer.recognize_google_cloud(audio, language='en-US').lower()

    def _recognize_sphinx(self, audio):
        """Offline Sphinx recognition"""
        return self.recognizer.recognize_sphinx(audio).lower()

    def _recognize_wit(self, audio):
        """Wit.ai recognition (if API key available)"""
        # Add your Wit.ai key if available
        WIT_AI_KEY = ""  # Add your key here
        if WIT_AI_KEY:
            return self.recognizer.recognize_wit(audio, key=WIT_AI_KEY).lower()
        return None

    def _clean_recognized_text(self, text):
        """Clean and normalize recognized text"""
        import re
        
        # Convert to lowercase
        text = text.lower().strip()
        
        # Remove filler words and normalize
        filler_words = ['um', 'uh', 'er', 'ah', 'like']
        words = text.split()
        words = [word for word in words if word not in filler_words]
        
        # Fix common recognition errors
        corrections = {
            'jarvis': ['jervis', 'jarvish', 'jarvas'],
            'chrome': ['crome', 'chrom'],
            'firefox': ['fire fox', 'firefix'],
            'calculator': ['calc', 'calculator'],
            'youtube': ['you tube', 'youtub'],
            'screenshot': ['screen shot', 'screan shot'],
            'volume': ['volum', 'valume'],
            'open': ['oper', 'oppen'],
            'close': ['clos', 'cloes'],
            'switch': ['swich', 'swithc']
        }
        
        # Apply corrections
        corrected_words = []
        for word in words:
            corrected = False
            for correct, variants in corrections.items():
                if word in variants:
                    corrected_words.append(correct)
                    corrected = True
                    break
            if not corrected:
                corrected_words.append(word)
        
        return ' '.join(corrected_words)

    def process_command(self, command):
        """Enhanced command processing with AI understanding"""
        if not command:
            return True
        
        # Store command in history
        self.command_history.append({
            'command': command,
            'timestamp': datetime.datetime.now()
        })
        self.last_command = command
        
        # Log command
        print(f"📝 Command: {command}")
        
        # Try AI processing first if enabled
        if self.ai_enabled:
            print("🧠 Processing with AI...")
            ai_result = self.ai.process_command(command)
            
            if ai_result['success']:
                # Speak AI response
                self.speak(ai_result['response'])
                
                # Execute extracted actions
                for action in ai_result['actions']:
                    self.execute_ai_action(action)
                
                # AI handled it successfully
                return True
            else:
                print("⚠️ AI processing failed, falling back to rule-based")
        
        # Fallback to original rule-based processing
        return self.process_command_classic(command)
    
    def process_command_classic(self, command):
        """Original rule-based command processing (fallback)"""
        # Your existing command processing code
        if "next window" in command or "alt tab" in command:
            self.switch_windows_alt_tab("forward")
            return True
        # ... rest of your existing check_* methods
        
        if self.check_system_commands(command):
            return True
        elif self.check_application_commands(command):
            return True
        elif self.check_web_commands(command):
            return True
        elif self.check_utility_commands(command):
            return True
        elif self.check_information_commands(command):
            return True
        elif self.check_control_commands(command):
            return True
        else:
            self.handle_unknown_command(command)
            return True

    def execute_ai_action(self, action: Dict):
        """Execute actions extracted by AI"""
        action_type = action.get('type', '').upper()
        
        print(f"🎯 Executing AI action: {action_type}")
        
        try:
            if action_type == "OPEN_APP":
                app = action.get('app', '')
                self.open_application_smart(app)
                
            elif action_type == "SEARCH_WEB":
                query = action.get('query', '')
                webbrowser.open(f"https://google.com/search?q={query}")
                
            elif action_type == "SEARCH_YOUTUBE":
                query = action.get('query', '')
                webbrowser.open(f"https://youtube.com/results?search_query={query}")
                
            elif action_type == "OPEN_WEBSITE":
                url = action.get('url', '')
                webbrowser.open(url)
                
            elif action_type == "TYPE_TEXT":
                text = action.get('text', '')
                self.type_text(text)
                
            elif action_type == "TAKE_SCREENSHOT":
                self.take_screenshot()
                
            elif action_type == "SYSTEM_INFO":
                info_type = action.get('params', '').lower()
                if 'battery' in info_type:
                    self.get_battery_status()
                elif 'cpu' in info_type:
                    cpu_percent = psutil.cpu_percent(interval=1)
                    self.speak(f"CPU usage is at {cpu_percent} percent")
                elif 'memory' in info_type:
                    memory = psutil.virtual_memory()
                    self.speak(f"Memory usage is at {memory.percent} percent")
                    
            elif action_type == "OPEN_FOLDER":
                folder = action.get('folder', '')
                self.open_folder_smart(folder)
                
            elif action_type == "CONTROL_VOLUME":
                direction = action.get('direction', '')
                if 'up' in direction:
                    self.adjust_volume(10)
                elif 'down' in direction:
                    self.adjust_volume(-10)
                elif 'mute' in direction:
                    self.toggle_mute(True)
                    
            elif action_type == "SWITCH_APP":
                app = action.get('app', '')
                self.switch_to_application(app)
                
            elif action_type == "WINDOW_CONTROL":
                operation = action.get('operation', '')
                self.control_window(operation)
                
        except Exception as e:
            print(f"❌ Error executing AI action: {e}")

    def switch_to_application(self, app_name: str):
        """Enhanced window switching with better detection and fallback"""
        try:
            print(f"🔍 Looking for application: {app_name}")
            
            # Get all windows efficiently
            windows = []
            try:
                for window in gw.getAllWindows():
                    if window.title and window.title.strip() and not window.title.isspace():
                        windows.append(window)
            except Exception as e:
                print(f"Error getting windows: {e}")
                return False
            
            if not windows:
                self.speak(f"No windows found. Opening {app_name} instead.")
                self.open_application_smart(app_name)
                return True
            
            # Enhanced app keyword mapping
            app_keywords = {
                'chrome': ['chrome', 'google chrome', 'chromium'],
                'firefox': ['firefox', 'mozilla firefox'],
                'edge': ['edge', 'microsoft edge'],
                'code': ['visual studio code', 'vscode', 'vs code', 'code'],
                'notepad': ['notepad', 'notepad++'],
                'explorer': ['explorer', 'file explorer', 'windows explorer'],
                'spotify': ['spotify'],
                'discord': ['discord'],
                'terminal': ['terminal', 'command prompt', 'cmd', 'powershell', 'windows terminal'],
                'outlook': ['outlook', 'microsoft outlook'],
                'teams': ['teams', 'microsoft teams'],
                'slack': ['slack'],
                'excel': ['excel', 'microsoft excel'],
                'word': ['word', 'microsoft word'],
                'powerpoint': ['powerpoint', 'microsoft powerpoint'],
                'zoom': ['zoom', 'zoom meetings'],
                'whatsapp': ['whatsapp'],
                'telegram': ['telegram'],
                'calculator': ['calculator', 'calc'],
                'paint': ['paint', 'mspaint', 'microsoft paint']
            }
            
            # Find matching keywords
            target_keywords = app_keywords.get(app_name.lower(), [app_name.lower()])
            
            print(f"🔎 Searching with keywords: {target_keywords}")
            
            # Score-based matching for better accuracy
            best_match = None
            best_score = 0
            
            for window in windows:
                window_title_lower = window.title.lower()
                
                # Calculate match score
                score = 0
                for keyword in target_keywords:
                    if keyword == window_title_lower:
                        score += 100  # Exact match
                    elif keyword in window_title_lower:
                        score += 50   # Contains keyword
                    elif any(part in window_title_lower for part in keyword.split()):
                        score += 25   # Partial word match
                
                if score > best_score:
                    best_score = score
                    best_match = window
            
            # Activate best match if score is good enough
            if best_match and best_score >= 25:
                try:
                    print(f"✅ Found window: {best_match.title} (score: {best_score})")
                    
                    # Enhanced activation sequence
                    if best_match.isMinimized:
                        best_match.restore()
                        time.sleep(0.1)
                    
                    best_match.activate()
                    time.sleep(0.2)
                    
                    # Force focus with click if needed
                    try:
                        center_x = best_match.left + best_match.width // 2
                        center_y = best_match.top + best_match.height // 2
                        pyautogui.click(center_x, center_y)
                    except:
                        pass
                    
                    self.speak(f"Switched to {app_name}")
                    return True
                    
                except Exception as e:
                    print(f"⚠️ Could not activate {best_match.title}: {e}")
            
            # If no good match found
            print(f"❌ No suitable window found for: {app_name}")
            self.speak(f"{app_name} doesn't appear to be running. Should I open it?")
            return False
            
        except Exception as e:
            print(f"❌ Window switching error: {e}")
            self.speak(f"Could not switch to {app_name}")
            return False

    def open_application_smart(self, app_name: str):
        """Smart application opener with fuzzy matching"""
        app_map = {
            'chrome': 'chrome',
            'firefox': 'firefox',
            'edge': 'msedge',
            'notepad': 'notepad',
            'calculator': 'calc',
            'vscode': 'code',
            'visual studio': 'code',
            'spotify': 'spotify',
            'discord': 'discord',
            'terminal': 'cmd',
            'command prompt': 'cmd',
            'explorer': 'explorer',
            'file explorer': 'explorer'
        }
        
        # Fuzzy match
        for key, value in app_map.items():
            if key in app_name.lower():
                os.system(f"start {value}")
                return
        
        # Try direct launch
        os.system(f"start {app_name}")

    def open_folder_smart(self, folder_name: str):
        """Smart folder opener"""
        folder_map = {
            'documents': os.path.expanduser("~/Documents"),
            'downloads': os.path.expanduser("~/Downloads"),
            'desktop': os.path.expanduser("~/Desktop"),
            'pictures': os.path.expanduser("~/Pictures"),
            'videos': os.path.expanduser("~/Videos"),
            'music': os.path.expanduser("~/Music")
        }
        
        folder_path = folder_map.get(folder_name.lower(), os.path.expanduser("~"))
        os.startfile(folder_path)

    def type_text(self, text: str):
        """Type text using pyautogui"""
        try:
            pyautogui.typewrite(text, interval=0.05)
        except ImportError:
            self.speak("Text typing requires pyautogui. Please install it.")

    def control_window(self, operation: str):
        """Control current window with better error handling"""
        try:
            # Get the active window
            active_window = gw.getActiveWindow()
            
            if active_window:
                if 'minimize' in operation:
                    active_window.minimize()
                    self.speak("Window minimized")
                elif 'maximize' in operation:
                    active_window.maximize()
                    self.speak("Window maximized")
                elif 'close' in operation:
                    pyautogui.hotkey('alt', 'f4')
                    self.speak("Window closed")
                elif 'restore' in operation:
                    active_window.restore()
                    self.speak("Window restored")
            else:
                self.speak("No active window found")
                
        except ImportError:
            self.speak("Window control requires pyautogui and pygetwindow")
        except Exception as e:
            print(f"❌ Window control error: {e}")
            self.speak("Could not control the window")

    def check_system_commands(self, command):
        """Handle system-level commands"""
        if any(word in command for word in ["shutdown", "shut down", "power off"]):
            self.speak("Initiating system shutdown in 10 seconds. Say 'cancel' to abort.")
            threading.Thread(target=self.delayed_shutdown, args=(10,), daemon=True).start()
            return True
            
        elif "restart" in command or "reboot" in command:
            self.speak("Restarting your system in 5 seconds.")
            self.execute_system_command("shutdown /r /t 5" if self.system == "Windows" else "sudo reboot")
            return True
            
        elif "lock" in command:
            self.speak(self.get_random_response('acknowledgment'))
            if self.system == "Windows":
                os.system("rundll32.exe user32.dll,LockWorkStation")
            else:
                os.system("gnome-screensaver-command -l")  # For Linux
            return True
            
        elif "sleep mode" in command or "hibernate" in command:
            self.speak("Entering sleep mode.")
            if self.system == "Windows":
                os.system("rundll32.exe powrprof.dll,SetSuspendState 0,1,0")
            return True
            
        elif "cancel" in command and "shutdown" in command:
            if self.system == "Windows":
                os.system("shutdown /a")
            self.speak("Shutdown cancelled.")
            return True
            
        return False

    def check_application_commands(self, command):
        """Handle application launches and switching with better detection"""
        apps = {
            'notepad': ['notepad', 'note pad', 'text editor'],
            'calculator': ['calculator', 'calc'],
            'chrome': ['chrome', 'google chrome', 'browser'],
            'firefox': ['firefox', 'mozilla'],
            'edge': ['edge', 'microsoft edge'],
            'explorer': ['explorer', 'file explorer', 'files'],
            'cmd': ['command prompt', 'cmd', 'terminal'],
            'code': ['vs code', 'visual studio code', 'vscode', 'visual studio', 'visual studios'],
            'spotify': ['spotify', 'music'],
            'discord': ['discord'],
            'slack': ['slack'],
            'zoom': ['zoom'],
            'paint': ['paint', 'mspaint'],
            'word': ['word', 'microsoft word'],
            'excel': ['excel', 'microsoft excel'],
            'powerpoint': ['powerpoint', 'power point'],
            'outlook': ['outlook', 'mail'],
            'teams': ['teams', 'microsoft teams'],
            'notepad++': ['notepad++', 'notepad plus plus']
        }
        
        # Check for switch/focus commands FIRST
        if "switch to" in command or "focus" in command or "go to" in command:
            # First try to match with our predefined apps
            for app_cmd, keywords in apps.items():
                if any(keyword in command for keyword in keywords):
                    try:
                        # Get all windows
                        all_windows = gw.getAllTitles()
                        
                        # Filter out empty titles
                        all_windows = [w for w in all_windows if w.strip()]
                        
                        # Try to find matching window
                        found = False
                        for window_title in all_windows:
                            # Check if any keyword matches the window title
                            for keyword in keywords:
                                if keyword.lower() in window_title.lower():
                                    try:
                                        window = gw.getWindowsWithTitle(window_title)[0]
                                        if window.isMinimized:
                                            window.restore()
                                        window.activate()  # Bring to front
                                        found = True
                                        self.speak(f"Switching to {keywords[0]}")
                                        return True
                                    except Exception as e:
                                        print(f"Error activating window: {e}")
                                        continue
                        
                        # If window not found, inform user
                        if not found:
                            self.speak(f"{keywords[0]} is not running. Would you like me to open it?")
                            return True
                            
                    except Exception as e:
                        print(f"Error in window switching: {e}")
                        self.speak(f"Could not switch to {keywords[0]}")
                        return True
            
            # If no predefined app matched, try generic window search
            try:
                # Extract app name from command
                search_term = command.replace("switch to", "").replace("focus", "").replace("go to", "").replace("open", "").strip()
                
                if search_term:
                    all_windows = gw.getAllTitles()
                    all_windows = [w for w in all_windows if w.strip()]
                    
                    # Try fuzzy matching
                    for window_title in all_windows:
                        if search_term.lower() in window_title.lower():
                            try:
                                window = gw.getWindowsWithTitle(window_title)[0]
                                if window.isMinimized:
                                    window.restore()
                                window.activate()
                                self.speak(f"Switching to {search_term}")
                                return True
                            except:
                                continue
                    
                    # If still not found
                    self.speak(f"Could not find a window for {search_term}")
                    return True
                    
            except Exception as e:
                print(f"Generic window search error: {e}")
        
        # Handle OPEN commands (new instances)
        elif "open" in command or "launch" in command or "start" in command:
            for app_cmd, keywords in apps.items():
                if any(keyword in command for keyword in keywords):
                    self.speak(f"Opening {keywords[0]}...")
                    try:
                        if self.system == "Windows":
                            os.system(f"start {app_cmd}")
                        else:
                            subprocess.Popen([app_cmd])
                        return True
                    except Exception as e:
                        self.speak(f"Could not open {keywords[0]}. It may not be installed.")
                        return True
                
        return False

    def switch_windows_alt_tab(self, direction="forward"):
        """Use Alt+Tab to switch between windows"""
        try:
            if direction == "forward":
                pyautogui.keyDown('alt')
                pyautogui.press('tab')
                time.sleep(0.5)  # Hold to see window switcher
                pyautogui.keyUp('alt')
            else:  # backward
                pyautogui.keyDown('alt')
                pyautogui.keyDown('shift')
                pyautogui.press('tab')
                time.sleep(0.5)
                pyautogui.keyUp('shift')
                pyautogui.keyUp('alt')
                
            self.speak("Switched window")
            return True
        except Exception as e:
            print(f"Alt+Tab error: {e}")
            return False

    def check_web_commands(self, command):
        """Enhanced web browsing commands"""
        # Direct website access
        websites = {
            'youtube': 'https://youtube.com',
            'google': 'https://google.com',
            'gmail': 'https://gmail.com',
            'facebook': 'https://facebook.com',
            'twitter': 'https://twitter.com',
            'github': 'https://github.com',
            'stackoverflow': 'https://stackoverflow.com',
            'reddit': 'https://reddit.com',
            'linkedin': 'https://linkedin.com',
            'instagram': 'https://instagram.com',
            'whatsapp': 'https://web.whatsapp.com',
            'netflix': 'https://netflix.com',
            'amazon': 'https://amazon.com'
        }
        
        for site, url in websites.items():
            if site in command and ("open" in command or "go to" in command):
                self.speak(f"Opening {site}...")
                webbrowser.open(url)
                return True
        
        # Search commands
        if "search" in command or "google" in command:
            query = command.replace("search", "").replace("google", "").replace("for", "").strip()
            if query:
                self.speak(f"Searching for {query}...")
                webbrowser.open(f"https://www.google.com/search?q={query}")
                return True
                
        elif "youtube" in command and ("search" in command or "play" in command):
            query = command.replace("youtube", "").replace("search", "").replace("play", "").replace("on", "").strip()
            if query:
                self.speak(f"Searching YouTube for {query}...")
                webbrowser.open(f"https://www.youtube.com/results?search_query={query}")
                return True
                
        elif "wikipedia" in command:
            query = command.replace("wikipedia", "").replace("search", "").replace("for", "").strip()
            if query:
                self.speak(f"Searching Wikipedia for {query}...")
                webbrowser.open(f"https://en.wikipedia.org/wiki/{query}")
                return True
                
        return False

    def check_utility_commands(self, command):
        """Handle utility and file system commands"""
        # Volume controls
        if "volume" in command:
            if "up" in command or "increase" in command:
                self.adjust_volume(10)
                return True
            elif "down" in command or "decrease" in command:
                self.adjust_volume(-10)
                return True
            elif "mute" in command:
                self.toggle_mute(True)
                return True
            elif "unmute" in command:
                self.toggle_mute(False)
                return True
                
        # File operations
        elif "open folder" in command or "open directory" in command:
            if "documents" in command:
                os.startfile(os.path.expanduser("~/Documents"))
                self.speak("Opening Documents folder")
            elif "downloads" in command:
                os.startfile(os.path.expanduser("~/Downloads"))
                self.speak("Opening Downloads folder")
            elif "desktop" in command:
                os.startfile(os.path.expanduser("~/Desktop"))
                self.speak("Opening Desktop folder")
            else:
                os.startfile(os.path.expanduser("~"))
                self.speak("Opening home folder")
            return True
            
        # Screenshot
        elif "screenshot" in command or "screen shot" in command:
            self.take_screenshot()
            return True
            
        # Task manager
        elif "task manager" in command:
            if self.system == "Windows":
                os.system("taskmgr")
                self.speak("Opening Task Manager")
            return True
            
        return False

    def check_information_commands(self, command):
        """Handle information queries"""
        # Time and date
        if "time" in command:
            current_time = datetime.datetime.now().strftime("%I:%M %p")
            self.speak(f"The time is {current_time}")
            return True
            
        elif "date" in command or "today" in command:
            today = datetime.datetime.now().strftime("%A, %B %d, %Y")
            self.speak(f"Today is {today}")
            return True
            
        elif "weather" in command:
            self.get_weather()
            return True
            
        # System information
        elif "battery" in command or "power" in command:
            self.get_battery_status()
            return True
            
        elif "cpu" in command or "processor" in command:
            cpu_percent = psutil.cpu_percent(interval=1)
            self.speak(f"CPU usage is at {cpu_percent} percent")
            return True
            
        elif "memory" in command or "ram" in command:
            memory = psutil.virtual_memory()
            self.speak(f"Memory usage is at {memory.percent} percent")
            return True
            
        elif "disk space" in command or "storage" in command:
            disk = psutil.disk_usage('/')
            self.speak(f"Disk usage is at {disk.percent} percent")
            return True
            
        return False

    def check_control_commands(self, command):
        """Handle Jarvis control commands"""
        # Exit commands
        if any(word in command for word in ["exit", "quit", "goodbye", "bye", "stop jarvis", "shut down jarvis"]):
            farewell = [
                "Goodbye sir. Have a great day!",
                "Shutting down. Until next time!",
                "System going offline. Farewell!",
                "It's been a pleasure serving you. Goodbye!"
            ]
            self.speak(random.choice(farewell))
            self.is_running = False
            return True
            
        # Listening modes
        elif "continuous listening" in command or "always listen" in command:
            self.continuous_listening = True
            self.speak("Continuous listening mode activated")
            return True
            
        elif "stop listening" in command:
            self.continuous_listening = False
            self.speak("Continuous listening mode deactivated")
            return True
            
        # Help command
        elif "help" in command or "what can you do" in command:
            self.show_help()
            return True
            
        # Repeat last command
        elif "repeat" in command and "last" in command:
            if self.last_command:
                self.speak(f"Your last command was: {self.last_command}")
                self.process_command(self.last_command)
            else:
                self.speak("No previous command to repeat")
            return True
            
        return False

    def handle_unknown_command(self, command):
        """Handle unrecognized commands"""
        responses = [
            "I'm not sure how to help with that. Could you rephrase?",
            "I didn't quite understand that command. Could you try again?",
            "That's not in my command set yet. Would you like to try something else?",
            "I'm still learning that one. Can I help you with something else?"
        ]
        self.speak(random.choice(responses))

    def show_help(self):
        """Display available commands"""
        help_text = """
        Available commands include:
        - Open applications: 'Open Chrome', 'Open Notepad', 'Open Calculator'
        - Web browsing: 'Search for...', 'Open YouTube', 'Play music on YouTube'
        - System control: 'Lock computer', 'Shutdown', 'Restart', 'Take screenshot'
        - Information: 'What time is it?', 'What's the date?', 'Battery status'
        - Volume control: 'Volume up', 'Volume down', 'Mute', 'Unmute'
        - File operations: 'Open Documents folder', 'Open Downloads'
        - And much more! Just ask naturally.
        """
        self.speak("I can help you with many tasks. Check the console for a full list.")
        print(help_text)

    def adjust_volume(self, change):
        """Adjust system volume"""
        if self.system == "Windows":
            # Use nircmd if available, otherwise use system commands
            try:
                if change > 0:
                    os.system(f"nircmd changesysvolume {change * 655}")
                else:
                    os.system(f"nircmd changesysvolume {change * 655}")
                self.speak(f"Volume {'increased' if change > 0 else 'decreased'}")
            except:
                self.speak("Volume control requires nircmd to be installed")
        else:
            # Linux/Mac volume control
            os.system(f"amixer set Master {abs(change)}%{'+' if change > 0 else '-'}")

    def toggle_mute(self, mute):
        """Toggle system mute"""
        if self.system == "Windows":
            try:
                os.system(f"nircmd mutesysvolume {1 if mute else 0}")
                self.speak("Muted" if mute else "Unmuted")
            except:
                self.speak("Mute control requires nircmd to be installed")

    def take_screenshot(self):
        """Take a screenshot with better error handling"""
        timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Use D drive for screenshots
        screenshots_dir = "D:\\Screenshots"
        # Create Screenshots directory if it doesn't exist
        if not os.path.exists(screenshots_dir):
            try:
                os.makedirs(screenshots_dir)
            except Exception:
                screenshots_dir = "D:"  # Fallback to D drive root
        
        screenshot_path = os.path.join(screenshots_dir, f"jarvis_screenshot_{timestamp}.png")
        
        try:
            # Take screenshot
            print(f"📸 Taking screenshot...")
            screenshot = pyautogui.screenshot()
            screenshot.save(screenshot_path)
            
            self.speak(f"Screenshot saved to {os.path.basename(screenshot_path)}")
            print(f"✅ Screenshot saved: {screenshot_path}")
            
            # Optional: Open the screenshot
            if "open" in self.last_command or "show" in self.last_command:
                os.startfile(screenshot_path)
                
        except PermissionError:
            self.speak("Permission denied. Cannot save screenshot to Pictures folder.")
            print(f"❌ Permission error saving to: {screenshot_path}")
            
        except Exception as e:
            error_msg = str(e)
            print(f"❌ Screenshot error: {error_msg}")
            
            if "tkinter" in error_msg.lower():
                self.speak("Screenshot failed. Tkinter display issue detected.")
            elif "display" in error_msg.lower():
                self.speak("Screenshot failed. Display environment issue.")
            else:
                self.speak(f"Screenshot failed due to: {error_msg}")

    def get_battery_status(self):
        """Get battery information"""
        try:
            battery = psutil.sensors_battery()
            if battery:
                percent = battery.percent
                plugged = "plugged in" if battery.power_plugged else "not plugged in"
                time_left = ""
                if not battery.power_plugged and battery.secsleft != -1:
                    hours = battery.secsleft // 3600
                    minutes = (battery.secsleft % 3600) // 60
                    time_left = f" About {hours} hours and {minutes} minutes remaining."
                
                self.speak(f"Battery is at {percent} percent and {plugged}.{time_left}")
            else:
                self.speak("No battery information available. You might be on a desktop.")
        except Exception as e:
            self.speak("Could not retrieve battery information")

    def get_weather(self):
        """Get weather information (requires API key)"""
        self.speak("Weather feature requires an API key setup. Opening weather website instead.")
        webbrowser.open("https://weather.com")

    def delayed_shutdown(self, delay):
        """Delayed shutdown with cancellation option"""
        time.sleep(delay)
        if self.is_running:  # Check if not cancelled
            if self.system == "Windows":
                os.system("shutdown /s /t 0")

    def execute_system_command(self, command):
        """Execute system commands safely"""
        try:
            if self.system == "Windows":
                os.system(command)
            else:
                subprocess.run(command, shell=True)
        except Exception as e:
            print(f"❌ System command error: {e}")

    def play_sound(self, sound_type):
        """Play UI sounds (optional enhancement)"""
        # This is a placeholder for sound effects
        # You can add actual sound files later
        pass

    def check_wake_word(self, text):
        """Check if wake word is detected"""
        if text:
            return any(wake in text for wake in self.wake_words)
        return False

    def passive_listening_mode(self):
        """Listen for wake word before processing commands"""
        print("🎯 Passive listening mode active. Say 'Jarvis' to activate.")
        
        while self.is_running:
            try:
                # Listen for shorter duration in passive mode
                audio = self.listen_for_audio(timeout=1, phrase_limit=3)
                
                if audio:
                    text = self.recognize_speech(audio)
                    
                    if self.check_wake_word(text):
                        # Wake word detected
                        self.speak(self.get_random_response('listening'))
                        
                        # Now listen for actual command
                        command_audio = self.listen_for_audio(timeout=5, phrase_limit=10)
                        if command_audio:
                            command = self.recognize_speech(command_audio)
                            if command:
                                self.process_command(command)
                                
            except KeyboardInterrupt:
                break
            except Exception as e:
                print(f"❌ Passive listening error: {e}")
                time.sleep(1)

    def active_listening_mode(self):
        """Traditional active listening mode"""
        print("🎯 Active listening mode. Listening for all commands.")
        
        while self.is_running:
            try:
                # Listen continuously
                audio = self.listen_for_audio(timeout=5, phrase_limit=10)
                
                if audio:
                    command = self.recognize_speech(audio)
                    if command:
                        # Check if it's a wake word alone
                        if self.check_wake_word(command) and len(command.split()) <= 2:
                            self.speak(self.get_random_response('listening'))
                        else:
                            self.process_command(command)
                            
                # Small delay between listening sessions
                if not self.continuous_listening:
                    time.sleep(0.5)
                    
            except KeyboardInterrupt:
                break
            except Exception as e:
                print(f"❌ Active listening error: {e}")
                time.sleep(1)

    def run(self, mode="active"):
        """Enhanced main execution loop with better error recovery"""
        consecutive_errors = 0
        max_consecutive_errors = 5
        
        try:
            while self.is_running and consecutive_errors < max_consecutive_errors:
                try:
                    if mode == "passive":
                        self.passive_listening_mode()
                    else:
                        self.active_listening_mode()
                    
                    # Reset error counter on successful operation
                    consecutive_errors = 0
                    
                except KeyboardInterrupt:
                    print("\n⚠️ Interrupted by user")
                    break
                except Exception as e:
                    consecutive_errors += 1
                    print(f"❌ Error #{consecutive_errors}: {e}")
                    
                    if consecutive_errors < max_consecutive_errors:
                        self.speak("I encountered an error. Recovering...")
                        time.sleep(1)  # Brief pause before retry
                    else:
                        print("❌ Too many consecutive errors. Shutting down.")
                        self.speak("Too many errors occurred. Shutting down for safety.")
                        break
                        
        finally:
            self.cleanup()

    def cleanup(self):
        """Clean shutdown procedure"""
        try:
            self.is_running = False
            
            if hasattr(self, 'tts_manager'):
                self.tts_manager.cleanup()
            
            # Save command history if enabled
            if hasattr(self, 'command_history') and self.command_history:
                self._save_command_history()
            
            # Final farewell
            farewell_messages = [
                "System shutting down. Goodbye!",
                "Until next time. Stay safe!",
                "Jarvis going offline. Have a great day!"
            ]
            self.speak(random.choice(farewell_messages))
            
        except Exception as e:
            print(f"⚠️ Cleanup error: {e}")

    def _save_command_history(self):
        """Save command history to file"""
        try:
            import json
            history_file = Path.home() / ".jarvis" / "command_history.json"
            history_file.parent.mkdir(exist_ok=True)
            
            with open(history_file, 'w') as f:
                json.dump(self.command_history, f, indent=2, default=str)
                
        except Exception as e:
            print(f"⚠️ Could not save command history: {e}")

def signal_handler(signum, frame):
    """Handle Ctrl+C interrupt"""
    print("\n⚠️ Interrupt signal received. Shutting down...")
    sys.exit(0)

def main():
    """Main entry point"""
    # Set up signal handler for Ctrl+C
    signal.signal(signal.SIGINT, signal_handler)
    
    print("""
    ╔══════════════════════════════════════════╗
    ║     JARVIS - Advanced Voice Assistant    ║
    ║            Version 2.0 Enhanced           ║
    ╚══════════════════════════════════════════╝
    """)
    
    # Get user preference
    print("\nSelect listening mode:")
    print("1. Active (Always listening)")
    print("2. Passive (Wake word activation)")
    
    choice = input("\nEnter choice (1/2): ").strip()
    
    mode = "passive" if choice == "2" else "active"
    
    # Initialize and run Jarvis
    try:
        jarvis = EnhancedJarvis()
        
        print(f"\n🚀 Starting Jarvis in {mode} mode...")
        print("Press Ctrl+C to exit\n")
        
        jarvis.run(mode=mode)
        
    except KeyboardInterrupt:
        print("\n⚠️ Program interrupted by user")
    except Exception as e:
        print(f"\n❌ Fatal error: {e}")
    finally:
        print("Shutting down...")

if __name__ == "__main__":
    main()