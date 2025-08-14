import sounddevice as sd
import soundfile as sf
import os
import time

def record_reference_voice():
    """Record multiple voice samples for better reference"""
    print("=" * 50)
    print("JARVIS VOICE REFERENCE RECORDING")
    print("=" * 50)
    print("\nThis will record your voice reference for authentication.")
    print("For best results:")
    print("- Find a quiet environment")
    print("- Speak clearly and naturally")
    print("- Use your normal speaking voice")
    print("- Avoid background noise")
    print("\nWe'll record 3 samples and combine them for better accuracy.")
    
    input("\nPress Enter when ready...")
    
    sample_rate = 22050
    duration = 5
    samples = []
    
    for i in range(3):
        print(f"\n--- Recording Sample {i+1}/3 ---")
        print("Say something like: 'Hello Jarvis, this is my voice for authentication'")
        print("Recording will start in 3 seconds...")
        
        for countdown in range(3, 0, -1):
            print(f"{countdown}...")
            time.sleep(1)
        
        print("🔴 RECORDING NOW - SPEAK CLEARLY!")
        
        try:
            recording = sd.rec(
                int(duration * sample_rate), 
                samplerate=sample_rate, 
                channels=1,
                dtype='float64'
            )
            sd.wait()
            samples.append(recording)
            print("✅ Sample recorded successfully!")
            
        except Exception as e:
            print(f"❌ Error recording sample {i+1}: {e}")
            return False
        
        if i < 2:  # Don't wait after the last sample
            print("Please wait 2 seconds before the next recording...")
            time.sleep(2)
    
    # Save the best quality sample (you can modify this logic)
    print("\nSaving voice reference...")
    
    try:
        # Use the middle sample (usually most stable)
        best_sample = samples[1]
        sf.write("my_voice.wav", best_sample, sample_rate, subtype='PCM_16')
        
        # Also save all samples for backup
        for i, sample in enumerate(samples):
            sf.write(f"my_voice_sample_{i+1}.wav", sample, sample_rate, subtype='PCM_16')
        
        print("✅ Voice reference saved successfully!")
        print("📁 Files created:")
        print("   - my_voice.wav (main reference)")
        print("   - my_voice_sample_1.wav (backup)")
        print("   - my_voice_sample_2.wav (backup)")
        print("   - my_voice_sample_3.wav (backup)")
        
        return True
        
    except Exception as e:
        print(f"❌ Error saving voice reference: {e}")
        return False

def test_playback():
    """Test the recorded voice"""
    if os.path.exists("my_voice.wav"):
        print("\n" + "=" * 30)
        print("TESTING RECORDED VOICE")
        print("=" * 30)
        
        try:
            data, fs = sf.read("my_voice.wav")
            print("Playing back your recorded voice...")
            sd.play(data, fs)
            sd.wait()
            print("✅ Playback completed!")
            
            response = input("\nDoes the playback sound clear? (y/n): ").lower().strip()
            if response == 'y':
                print("✅ Great! Your voice reference is ready.")
                return True
            else:
                print("❌ You may want to re-record for better quality.")
                return False
                
        except Exception as e:
            print(f"❌ Error playing back audio: {e}")
            return False
    else:
        print("❌ No voice reference file found!")
        return False

def main():
    """Main function for recording voice reference"""
    print("🎤 Jarvis Voice Reference Setup")
    
    # Check if reference already exists
    if os.path.exists("my_voice.wav"):
        print("\n⚠️  Voice reference already exists!")
        response = input("Do you want to record a new one? (y/n): ").lower().strip()
        if response != 'y':
            print("Using existing voice reference.")
            return
    
    # Record new reference
    if record_reference_voice():
        # Test the recording
        if test_playback():
            print("\n🎉 Voice reference setup completed successfully!")
            print("You can now run the main Jarvis script.")
        else:
            rerecord = input("Would you like to try recording again? (y/n): ").lower().strip()
            if rerecord == 'y':
                main()  # Recursive call to try again
    else:
        print("\n❌ Failed to record voice reference. Please try again.")

if __name__ == "__main__":
    main()