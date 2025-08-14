import sounddevice as sd
import soundfile as sf
import speech_recognition as sr
import time
import os
import webbrowser
import datetime
from voice_matcher import EnhancedVoiceMatcher
import pyttsx3

engine = pyttsx3.init()

class JarvisVoiceAssistant:
    def __init__(self):
        self.voice_matcher = EnhancedVoiceMatcher()
        self.reference_trained = False
        self.failed_attempts = 0
        self.max_failed_attempts = 3
        self.recording_duration = 4  # Increased for better feature extraction
        self.sample_rate = 22050     # Consistent sample rate
        
    def speak(self, text):
        print("Jarvis:", text)
        engine.say(text)
        engine.runAndWait()

    def initialize_voice_reference(self):
        """Initialize voice reference if not already done"""
        if os.path.exists("my_voice.wav") and not self.reference_trained:
            print("Loading voice reference...")
            if self.voice_matcher.train_reference("my_voice.wav"):
                self.reference_trained = True
                self.speak("Voice reference loaded successfully")
                return True
            else:
                self.speak("Failed to load voice reference. Please record a new reference.")
                return False
        elif not os.path.exists("my_voice.wav"):
            self.speak("No voice reference found. Please record your voice first using record_reference.py")
            return False
        return True

    def record_for_match(self, filename="test_voice.wav"):
        """Record audio with improved quality"""
        print("Listening for command...")
        
        try:
            # Record with consistent parameters
            recording = sd.rec(
                int(self.recording_duration * self.sample_rate), 
                samplerate=self.sample_rate, 
                channels=1,
                dtype='float64'
            )
            sd.wait()
            
            # Save with high quality
            sf.write(filename, recording, self.sample_rate, subtype='PCM_16')
            print("Voice captured successfully.")
            return True
            
        except Exception as e:
            print(f"Error recording audio: {e}")
            return False

    def test_voice_matching_debug(self, test_file):
        """Debug version to see what's happening with voice matching"""
        print("\n" + "="*50)
        print("DEBUGGING VOICE MATCHING")
        print("="*50)
        
        # Test with very low thresholds
        thresholds_to_test = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6]
        
        for threshold in thresholds_to_test:
            print(f"\nTesting with threshold: {threshold}")
            result = self.voice_matcher.match_voice(test_file, threshold=threshold, detailed_output=True)
            if result:
                print(f"✅ MATCH found at threshold {threshold}!")
                return True, threshold
        
        print("❌ No match found at any threshold")
        return False, None

    def recognize_command(self, file="test_voice.wav"):
        """Recognize speech with better error handling"""
        r = sr.Recognizer()
        
        # Adjust for ambient noise
        try:
            with sr.AudioFile(file) as source:
                # Adjust for ambient noise
                r.adjust_for_ambient_noise(source, duration=0.5)
                audio = r.record(source)
            
            # Use Google Speech Recognition
            command = r.recognize_google(audio, language='en-US')
            print("Recognized command:", command)
            return command.lower()
            
        except sr.UnknownValueError:
            print("Could not understand audio")
            return "none"
        except sr.RequestError as e:
            print(f"Could not request results; {e}")
            return "none"
        except Exception as e:
            print(f"Error in speech recognition: {e}")
            return "none"

    def execute_command(self, command):
        """Execute voice commands with enhanced functionality"""
        if "open notepad" in command:
            os.system("start notepad")
            self.speak("Opening Notepad")
            
        elif "open chrome" in command or "open browser" in command:
            os.system("start chrome")
            self.speak("Opening Chrome")
            
        elif "open calculator" in command:
            os.system("start calc")
            self.speak("Opening Calculator")
            
        elif "open file explorer" in command or "open explorer" in command:
            os.system("start explorer")
            self.speak("Opening File Explorer")
            
        elif "open" in command and "folder" not in command:
            app_name = command.replace("open", "").strip()
            try:
                os.system(f"start {app_name}")
                self.speak(f"Opening {app_name}")
            except:
                self.speak("Sorry, I could not open the application.")
                
        elif "shutdown" in command or "shut down" in command:
            self.speak("Shutting down your system in 10 seconds. Say cancel to abort.")
            # Give user time to cancel
            time.sleep(3)
            os.system("shutdown /s /t 7")
            
        elif "restart" in command:
            self.speak("Restarting your system")
            os.system("shutdown /r /t 5")
            
        elif "lock" in command or "lock computer" in command:
            self.speak("Locking your computer")
            os.system("rundll32.exe user32.dll,LockWorkStation")
            
        elif "search" in command or "google" in command:
            query = command.replace("search", "").replace("google", "").strip()
            if query:
                webbrowser.open(f"https://www.google.com/search?q={query}")
                self.speak(f"Searching Google for {query}")
            else:
                self.speak("What would you like me to search for?")
                
        elif "youtube" in command:
            query = command.replace("youtube", "").replace("search", "").strip()
            if query:
                webbrowser.open(f"https://www.youtube.com/results?search_query={query}")
                self.speak(f"Searching YouTube for {query}")
            else:
                webbrowser.open("https://www.youtube.com")
                self.speak("Opening YouTube")
                
        elif "time" in command:
            now = datetime.datetime.now().strftime("%I:%M %p")
            self.speak(f"The time is {now}")
            
        elif "date" in command:
            today = datetime.datetime.now().strftime("%A, %B %d, %Y")
            self.speak(f"Today is {today}")
            
        elif "open folder" in command or "documents" in command:
            folder_path = os.path.expanduser("~/Documents")
            os.startfile(folder_path)
            self.speak("Opening your documents folder")
            
        elif "volume up" in command:
            os.system("nircmd changesysvolume 5000")
            self.speak("Volume increased")
            
        elif "volume down" in command:
            os.system("nircmd changesysvolume -5000")
            self.speak("Volume decreased")
            
        elif "mute" in command:
            os.system("nircmd mutesysvolume 1")
            self.speak("System muted")
            
        elif "unmute" in command:
            os.system("nircmd mutesysvolume 0")
            self.speak("System unmuted")
            
        elif "stop" in command or "exit" in command or "quit" in command:
            self.speak("Goodbye! Have a great day!")
            return False
            
        elif "cancel" in command:
            os.system("shutdown /a")  # Cancel any pending shutdown
            self.speak("Operation cancelled")
            
        else:
            self.speak("Sorry, I didn't understand that command. Please try again.")
            
        return True

    def run(self):
        """Main execution loop with improved error handling"""
        self.speak("Jarvis voice assistant starting up")
        
        # Initialize voice reference
        if not self.initialize_voice_reference():
            return
        
        self.speak("I'm ready for your commands")
        
        while True:
            try:
                # Record voice input
                if not self.record_for_match():
                    continue
                
                # DEBUG: Test voice matching with multiple thresholds
                is_user, best_threshold = self.test_voice_matching_debug("test_voice.wav")
                
                if is_user:
                    # Reset failed attempts on successful match
                    self.failed_attempts = 0
                    print(f"✅ Voice authenticated with threshold {best_threshold}")
                    
                    # Recognize and execute command
                    cmd = self.recognize_command()
                    if cmd != "none":
                        if not self.execute_command(cmd):
                            break  # Exit command received
                    else:
                        self.speak("I couldn't understand what you said. Please try again.")
                else:
                    self.failed_attempts += 1
                    print(f"❌ Voice not recognized. Access denied. (Attempt {self.failed_attempts}/{self.max_failed_attempts})")
                    
                    if self.failed_attempts >= self.max_failed_attempts:
                        self.speak("Too many failed voice authentication attempts. Please try again later.")
                        time.sleep(10)  # Cool-down period
                        self.failed_attempts = 0
                
                # Brief pause between iterations
                time.sleep(1)
                
            except KeyboardInterrupt:
                self.speak("Voice assistant shutting down")
                break
            except Exception as e:
                print(f"Unexpected error: {e}")
                time.sleep(2)

if __name__ == "__main__":
    jarvis = JarvisVoiceAssistant()
    jarvis.run()