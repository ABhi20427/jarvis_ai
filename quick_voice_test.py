#!/usr/bin/env python3
"""
Quick test to see if voice matching works at all
"""

import os
from voice_matcher import EnhancedVoiceMatcher

def quick_voice_test():
    print("=" * 50)
    print("QUICK VOICE MATCHING TEST")
    print("=" * 50)
    
    # Check if reference voice exists
    if not os.path.exists("my_voice.wav"):
        print("❌ Error: No reference voice found!")
        print("Please run 'python record_reference.py' first")
        return
    
    print("Testing if reference voice matches itself...")
    
    # Test with multiple thresholds
    matcher = EnhancedVoiceMatcher()
    
    if matcher.train_reference("my_voice.wav"):
        print("Reference training successful!")
        
        # Test various thresholds
        thresholds = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
        
        print("\nTesting self-match with different thresholds:")
        print("-" * 50)
        
        for threshold in thresholds:
            result = matcher.match_voice("my_voice.wav", threshold=threshold, detailed_output=False)
            status = "✅ PASS" if result else "❌ FAIL"
            print(f"Threshold {threshold:.1f}: {status}")
        
        # Get the actual score
        print("\nDetailed scoring:")
        matcher.match_voice("my_voice.wav", threshold=0.5, detailed_output=True)
        
    else:
        print("❌ Failed to train reference voice!")

if __name__ == "__main__":
    quick_voice_test()