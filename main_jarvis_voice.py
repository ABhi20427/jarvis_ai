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
        """Initialize voice reference with multiple samples"""
        reference_files = [
            "my_voice.wav",
            "my_voice_sample_1.wav", 
            "my_voice_sample_2.wav",
            "my_voice_sample_3.wav"
        ]
        
        # Check if we have at least the main reference
        if os.path.exists("my_voice.wav"):
            # Find all available reference files
            available_files = [f for f in reference_files if os.path.exists(f)]
            
            print(f"Found {len(available_files)} reference files")
            
            if self.voice_matcher.train_multiple_references(available_files):
                self.reference_trained = True
                self.speak(f"Voice reference loaded with {len(available_files)} samples")
                return True
            else:
                self.speak("Failed to load voice reference. Please record new samples.")
                return False
        else:
            self.speak("No voice reference found. Please run record_reference.py first")
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

    def test_voice_matching_secure(self, test_file):
        """Secure voice matching with stricter thresholds"""
        print("\n" + "="*30)
        print("SECURE VOICE AUTHENTICATION")
        print("="*30)
        
        # Use higher, more secure thresholds
        primary_threshold = 0.75  # Primary security threshold
        fallback_threshold = 0.65  # Fallback threshold
        
        # Test with enhanced features first, fall back to basic
        try:
            test_features = self.voice_matcher.extract_enhanced_features(test_file)
            if test_features is None:
                test_features = self.voice_matcher.extract_basic_features(test_file)
        except:
            test_features = self.voice_matcher.extract_basic_features(test_file)
        
        if test_features is None:
            print("❌ Could not extract voice features")
            return False, 0.0
        
        # Use advanced similarity if available, otherwise fall back to basic
        try:
            if hasattr(self.voice_matcher, 'reference_samples'):
                similarity_score = self.voice_matcher.calculate_advanced_similarity_score(test_features)
            else:
                similarity_score = self.voice_matcher.calculate_similarity_score(test_features)
        except Exception as e:
            print(f"Error in similarity calculation: {e}")
            similarity_score = self.voice_matcher.calculate_similarity_score(test_features)
        
        print(f"Voice similarity score: {similarity_score:.4f}")
        print(f"Primary threshold: {primary_threshold}")
        print(f"Fallback threshold: {fallback_threshold}")
        
        if similarity_score >= primary_threshold:
            print("✅ HIGH CONFIDENCE MATCH")
            return True, similarity_score
        elif similarity_score >= fallback_threshold:
            print("⚠️  MEDIUM CONFIDENCE MATCH")
            return True, similarity_score
        else:
            print("❌ AUTHENTICATION FAILED")
            return False, similarity_score

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
                is_user, confidence_score = self.test_voice_matching_secure("test_voice.wav")
                
                if is_user:
                    # Reset failed attempts on successful match
                    self.failed_attempts = 0
                    print(f"✅ Voice authenticated with confidence score {confidence_score:.4f}")
                    
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