
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
        
        # Wake words
        self.wake_words = ["jarvis", "hey jarvis", "okay jarvis", "computer"]
        
        # Initialize responses
        self.init_responses()
        
        print("🤖 Enhanced Jarvis System Initializing...")
        self.startup_sequence()

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
        """Simple and reliable TTS method"""
        print(f"🔊 Jarvis: {text}")
        
        try:
            # Create a fresh engine instance for each speak operation
            # This prevents blocking issues
            speak_engine = pyttsx3.init()
            speak_engine.setProperty('rate', 180)
            speak_engine.setProperty('volume', 0.9)
            
            if priority:
                # For priority messages, we just speak immediately
                speak_engine.say(text)
                speak_engine.runAndWait()
            elif wait:
                # Blocking speech
                speak_engine.say(text)
                speak_engine.runAndWait()
            else:
                # Non-blocking speech in a thread
                def speak_async():
                    async_engine = pyttsx3.init()
                    async_engine.setProperty('rate', 180)
                    async_engine.say(text)
                    async_engine.runAndWait()
                
                threading.Thread(target=speak_async, daemon=True).start()
                
        except Exception as e:
            print(f"❌ TTS Error: {e}")
            # Fallback: at least print the message
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
        """Enhanced command processing with context awareness"""
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
        
        # Add window switching commands
        if "next window" in command or "alt tab" in command:
            self.switch_windows_alt_tab("forward")
            return True
        elif "previous window" in command:
            self.switch_windows_alt_tab("backward")
            return True
        
        # Process based on command categories
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