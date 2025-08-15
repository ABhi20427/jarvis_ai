
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

# Add after existing imports
from ai_brain_local import JarvisLocalAI
from config import AIConfig

# Initialize TTS engine with better error handling
try:
    engine = pyttsx3.init()
    engine.setProperty('rate', 180)
    engine.setProperty('volume', 0.9)
    # Test the engine immediately
    engine.say("Initializing")
    engine.runAndWait()
except Exception as e:
    print(f"TTS Engine initialization error: {e}")
    engine = None

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
        
        # Initialize AI Brain
        self.init_ai_brain()
        
        # Wake words
        self.wake_words = ["jarvis", "hey jarvis", "okay jarvis", "computer"]
        
        # Initialize responses
        self.init_responses()
        
        print("🤖 Enhanced Jarvis System Initializing...")
        self.startup_sequence()

    def init_ai_brain(self):
        """Initialize the AI brain"""
        if AIConfig.ENABLE_AI:
            try:
                self.ai = JarvisLocalAI(model=AIConfig.AI_MODEL)
                if self.ai.is_available:
                    self.ai_enabled = True
                    print("🧠 AI Brain initialized successfully")
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

    def startup_sequence(self):
        """Jarvis-like startup sequence"""
        startup_messages = [
            "System initialization complete.",
            f"Welcome back, {self.user_name}.",
            f"All systems operational. Current time is {datetime.datetime.now().strftime('%I:%M %p')}.",
            "Ready for your commands."
        ]
        
        for message in startup_messages:
            print(f"🤖 {message}")
            self.speak(message, wait=False)
            time.sleep(0.5)

    def speak(self, text, wait=True, priority=False):
        """Fixed TTS method without run loop conflicts"""
        print(f"🔊 Jarvis: {text}")
        
        try:
            # Method: Use a separate thread for each speak operation
            def speak_thread():
                try:
                    # Create a fresh engine for this operation
                    temp_engine = pyttsx3.init()
                    temp_engine.setProperty('rate', 180)
                    temp_engine.setProperty('volume', 0.9)
                    temp_engine.say(text)
                    temp_engine.runAndWait()
                    # Important: Clean up the engine
                    temp_engine.stop()
                    del temp_engine
                except RuntimeError as e:
                    if "run loop already started" in str(e):
                        # If run loop error, just print
                        print(f"[TTS Skipped - Engine Busy]: {text}")
                    else:
                        print(f"TTS Thread Error: {e}")
                except Exception as e:
                    print(f"TTS Error: {e}")
            
            if wait:
                # Blocking speech - run in current thread
                speak_thread()
            else:
                # Non-blocking speech - run in separate thread
                import threading
                thread = threading.Thread(target=speak_thread, daemon=True)
                thread.start()
                if priority:
                    thread.join()  # Wait for priority messages
                    
        except Exception as e:
            print(f"❌ TTS Error: {e}")
            print(f"[TTS Failed] Would have said: {text}")

    def get_random_response(self, response_type):
        """Get varied responses for natural interaction"""
        return random.choice(self.responses.get(response_type, ["Processing..."]))

    def listen_for_audio(self, timeout=None, phrase_limit=None):
        """Enhanced listening with better error handling"""
        try:
            with sr.Microphone(sample_rate=self.sample_rate) as source:
                # Adjust for ambient noise quickly
                self.recognizer.adjust_for_ambient_noise(source, duration=0.5)
                
                print("🎤 Listening...")
                self.is_listening = True
                
                # Add visual/audio feedback
                self.play_sound("listening")  # Optional: Add a listening sound
                
                audio = self.recognizer.listen(
                    source,
                    timeout=timeout,
                    phrase_time_limit=phrase_limit
                )
                
                self.is_listening = False
                return audio
                
        except sr.WaitTimeoutError:
            self.is_listening = False
            return None
        except Exception as e:
            print(f"❌ Microphone error: {e}")
            self.is_listening = False
            return None

    def recognize_speech(self, audio):
        """Enhanced speech recognition with multiple engines fallback"""
        if audio is None:
            return None
            
        try:
            # Primary: Google Speech Recognition
            text = self.recognizer.recognize_google(audio, language='en-US')
            return text.lower()
            
        except sr.UnknownValueError:
            # Try alternative recognition if Google fails
            try:
                # Fallback to Sphinx (offline)
                text = self.recognizer.recognize_sphinx(audio)
                return text.lower()
            except:
                return None
                
        except sr.RequestError as e:
            print(f"❌ Recognition service error: {e}")
            self.speak("I'm having trouble with the recognition service.")
            return None
            
        except Exception as e:
            print(f"❌ Unexpected recognition error: {e}")
            return None

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
        """Enhanced window switching with better app detection"""
        try:
            import pygetwindow as gw
            import pyautogui
            import time
            
            print(f"🔍 Looking for application: {app_name}")
            
            # Get all windows
            windows = gw.getAllWindows()
            
            # App name mappings for better matching
            app_keywords = {
                'chrome': ['chrome', 'google chrome'],
                'firefox': ['firefox', 'mozilla'],
                'edge': ['edge', 'microsoft edge'],
                'code': ['visual studio code', 'vscode', 'vs code', 'code'],
                'notepad': ['notepad'],
                'explorer': ['explorer', 'file explorer'],
                'spotify': ['spotify'],
                'discord': ['discord'],
                'terminal': ['terminal', 'command prompt', 'cmd', 'powershell'],
                'outlook': ['outlook'],
                'teams': ['teams', 'microsoft teams'],
                'slack': ['slack'],
                'excel': ['excel'],
                'word': ['word'],
                'powerpoint': ['powerpoint'],
                'whatsapp': ['whatsapp'],
                'telegram': ['telegram']
            }
            
            # Find matching keyword set
            target_keywords = []
            for app, keywords in app_keywords.items():
                if any(kw in app_name.lower() for kw in keywords):
                    target_keywords = keywords
                    break
            
            if not target_keywords:
                target_keywords = [app_name.lower()]
            
            print(f"🔎 Searching with keywords: {target_keywords}")
            
            # Try to find and activate the window
            for window in windows:
                if window.title:  # Skip windows with empty titles
                    window_title_lower = window.title.lower()
                    
                    # Check if any keyword matches
                    for keyword in target_keywords:
                        if keyword in window_title_lower:
                            try:
                                print(f"✅ Found window: {window.title}")
                                
                                # Different activation methods for reliability
                                if window.isMinimized:
                                    window.restore()
                                
                                window.activate()
                                time.sleep(0.1)  # Small delay for window to activate
                                
                                # Force focus with pyautogui click
                                if window.left >= 0 and window.top >= 0:
                                    pyautogui.click(window.left + 100, window.top + 50)
                                
                                self.speak(f"Switched to {app_name}")
                                return True
                                
                            except Exception as e:
                                print(f"⚠️ Could not activate {window.title}: {e}")
                                continue
            
            # If not found, try to open it
            print(f"❌ Window not found for: {app_name}")
            self.speak(f"{app_name} is not running. Let me open it for you.")
            self.open_application_smart(app_name)
            return False
            
        except ImportError:
            self.speak("Window switching requires pygetwindow. Please install it.")
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
            import pyautogui
            pyautogui.typewrite(text, interval=0.05)
        except ImportError:
            self.speak("Text typing requires pyautogui. Please install it.")
    
    
    def control_window(self, operation: str):
        """Control current window with better error handling"""
        try:
            import pyautogui
            import pygetwindow as gw
            
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
            'code': ['vs code', 'visual studio code', 'vscode', 'visual studio', 'visual studios'],  # Added visual studio
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
        if "switch to" in command or "focus" in command or "go to" in command or "open" in command:
            # Import here to avoid errors if not installed
            try:
                import pygetwindow as gw
            except ImportError:
                self.speak("Window switching requires pygetwindow. Please install it with: pip install pygetwindow")
                return False
            
            # First try to match with our predefined apps
            for app_cmd, keywords in apps.items():
                if any(keyword in command for keyword in keywords):
                    if "switch to" in command or "focus" in command or "go to" in command:
                        # Try to switch to existing window
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
                                            window.activate()  # Bring to front
                                            window.maximize()  # Optional: maximize the window
                                            self.speak(f"Switching to {keywords[0]}")
                                            return True
                                        except Exception as e:
                                            print(f"Error activating window: {e}")
                                            continue
                            
                            # If window not found, try to open the app
                            if not found:
                                self.speak(f"{keywords[0]} is not running. Opening it now...")
                                os.system(f"start {app_cmd}")
                                return True
                                
                        except Exception as e:
                            print(f"Error in window switching: {e}")
                            self.speak(f"Could not switch to {keywords[0]}")
                            return True
                    
                    elif "open" in command or "launch" in command or "start" in command:
                        # Open new instance
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
                                window.activate()
                                window.maximize()
                                self.speak(f"Switching to {search_term}")
                                return True
                            except:
                                continue
                    
                    # If still not found
                    self.speak(f"Could not find a window for {search_term}")
                    return True
                    
            except Exception as e:
                print(f"Generic window search error: {e}")
                
        return False

    def switch_windows_alt_tab(self, direction="forward"):
        """Use Alt+Tab to switch between windows"""
        try:
            import pyautogui
            
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
            # Check if pyautogui is available
            try:
                import pyautogui
            except ImportError:
                self.speak("Screenshot feature requires pyautogui to be installed. Install it with: pip install pyautogui")
                return
            
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
                audio = self.listen_for_audio(timeout=None, phrase_limit=10)
                
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
        """Main execution loop with different modes"""
        try:
            if mode == "passive":
                self.passive_listening_mode()
            else:
                self.active_listening_mode()
                
        except KeyboardInterrupt:
            print("\n⚠️ Interrupted by user")
        except Exception as e:
            print(f"❌ Critical error: {e}")
        finally:
            self.speak("System shutting down. Goodbye!")
            self.is_running = False

def main():
    """Main entry point"""
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
    jarvis = EnhancedJarvis()
    
    print(f"\n🚀 Starting Jarvis in {mode} mode...")
    print("Press Ctrl+C to exit\n")
    
    jarvis.run(mode=mode)

if __name__ == "__main__":
    main()