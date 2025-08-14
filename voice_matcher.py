import librosa
import numpy as np
import os
try:
    from sklearn.preprocessing import StandardScaler
    from sklearn.metrics.pairwise import cosine_similarity
    from scipy.spatial.distance import euclidean
    SKLEARN_AVAILABLE = True
except ImportError:
    SKLEARN_AVAILABLE = False
    print("Warning: scikit-learn not available, using basic similarity metrics")

import warnings
warnings.filterwarnings("ignore")

class EnhancedVoiceMatcher:
    def __init__(self):
        if SKLEARN_AVAILABLE:
            self.scaler = StandardScaler()
        self.reference_features = None

    def train_multiple_references(self, reference_files):
        """Train with multiple reference voice samples for better accuracy"""
        print("Training with multiple reference voices...")
        all_features = []
        
        for ref_file in reference_files:
            if os.path.exists(ref_file):
                # Try enhanced features first, fall back to basic if needed
                features = self.extract_enhanced_features(ref_file)
                if features is None:
                    features = self.extract_basic_features(ref_file)
                
                if features is not None:
                    all_features.append(features)
                    print(f"✅ Processed {ref_file}")
                else:
                    print(f"❌ Failed to process {ref_file}")
        
        if len(all_features) == 0:
            print("No valid reference samples found!")
            return False
        
        # Create reference profile using statistics from multiple samples
        all_features = np.array(all_features)
        self.reference_mean = np.mean(all_features, axis=0)
        self.reference_std = np.std(all_features, axis=0)
        self.reference_samples = all_features
        
        if SKLEARN_AVAILABLE:
            # Fit scaler on all samples
            self.scaler.fit(all_features)
            self.reference_samples = self.scaler.transform(all_features)
            self.reference_mean = np.mean(self.reference_samples, axis=0)
            self.reference_std = np.std(self.reference_samples, axis=0)
        
        print(f"Reference voice trained with {len(all_features)} samples")
        return True

    def preprocess_audio(self, audio_file):
        """Basic audio preprocessing that works with any librosa version"""
        try:
            # Load audio
            y, sr = librosa.load(audio_file, sr=22050)
            
            # Basic trimming if available
            try:
                y_trimmed, _ = librosa.effects.trim(y, top_db=20)
            except:
                y_trimmed = y  # Skip trimming if not available
            
            # Basic normalization
            y_normalized = y_trimmed / np.max(np.abs(y_trimmed)) if np.max(np.abs(y_trimmed)) > 0 else y_trimmed
            
            return y_normalized, sr
        except Exception as e:
            print(f"Error in audio preprocessing: {e}")
            return None, None
    def extract_basic_features(self, audio_file):
        """Extract only the most basic features that work with any librosa version"""
        y, sr = self.preprocess_audio(audio_file)
        if y is None:
            return None

        features = []

        try:
            # MFCC features - this should work with any librosa version
            mfcc = librosa.feature.mfcc(y=y, sr=sr, n_mfcc=13)
            mfcc_mean = np.mean(mfcc, axis=1)  # Mean across time
            mfcc_std = np.std(mfcc, axis=1)    # Std across time
            features.extend(mfcc_mean)
            features.extend(mfcc_std)
            print(f"MFCC features extracted: {len(mfcc_mean) + len(mfcc_std)}")

            # Basic spectral features
            # Zero crossing rate
            zcr = np.mean(librosa.feature.zero_crossing_rate(y))
            features.append(zcr)

            # RMS energy
            rms = np.mean(librosa.feature.rms(y=y))
            features.append(rms)

            # Spectral centroid (if available)
            try:
                spec_cent = np.mean(librosa.feature.spectral_centroid(y=y, sr=sr))
                features.append(spec_cent)
            except:
                features.append(sr/4)  # Fallback value

            # Simple pitch estimation using autocorrelation
            autocorr = np.correlate(y, y, mode='full')
            autocorr = autocorr[autocorr.size // 2:]

            # Find fundamental frequency
            try:
                pitch_idx = np.argmax(autocorr[sr//300:sr//50]) + sr//300  # Look for pitch between 50-300 Hz
                pitch = sr / pitch_idx if pitch_idx > 0 else 100
                features.append(pitch)
            except:
                features.append(100)  # Fallback pitch

            print(f"Total features extracted: {len(features)}")
            return np.array(features)

        except Exception as e:
            print(f"Error extracting features: {e}")
            return None

    def extract_enhanced_features(self, audio_file):
        """Extract comprehensive features for better voice discrimination"""
        y, sr = self.preprocess_audio(audio_file)
        if y is None:
            return None
        
        features = []
        
        try:
            # MFCC features (more comprehensive)
            mfcc = librosa.feature.mfcc(y=y, sr=sr, n_mfcc=20)  # Increased from 13
            mfcc_mean = np.mean(mfcc, axis=1)
            mfcc_std = np.std(mfcc, axis=1)
            try:
                mfcc_delta = np.mean(librosa.feature.delta(mfcc), axis=1)  # Velocity
                features.extend(mfcc_delta)
            except:
                features.extend(np.zeros(20))  # Fallback if delta fails
            
            features.extend(mfcc_mean)
            features.extend(mfcc_std)
            
            # Spectral features
            try:
                spectral_centroids = librosa.feature.spectral_centroid(y=y, sr=sr)
                features.append(np.mean(spectral_centroids))
                features.append(np.std(spectral_centroids))
                
                spectral_bandwidth = librosa.feature.spectral_bandwidth(y=y, sr=sr)
                features.append(np.mean(spectral_bandwidth))
                features.append(np.std(spectral_bandwidth))
                
                spectral_rolloff = librosa.feature.spectral_rolloff(y=y, sr=sr)
                features.append(np.mean(spectral_rolloff))
                features.append(np.std(spectral_rolloff))
            except:
                features.extend([1000, 500, 2000, 1000, 3000, 1500])  # Fallback values
            
            # Zero crossing rate
            zcr = librosa.feature.zero_crossing_rate(y)
            features.append(np.mean(zcr))
            features.append(np.std(zcr))
            
            # RMS energy
            rms = librosa.feature.rms(y=y)
            features.append(np.mean(rms))
            features.append(np.std(rms))
            
            # Basic pitch estimation (keeping it simple)
            try:
                autocorr = np.correlate(y, y, mode='full')
                autocorr = autocorr[autocorr.size // 2:]
                pitch_idx = np.argmax(autocorr[sr//300:sr//50]) + sr//300
                pitch = sr / pitch_idx if pitch_idx > 0 else 150
                features.append(pitch)
                features.append(50.0)  # Pitch std placeholder
            except:
                features.extend([150.0, 50.0])  # Fallback values
            
            # Add some spectral shape features
            try:
                stft = np.abs(librosa.stft(y))
                spectral_mean = np.mean(stft)
                spectral_std = np.std(stft)
                features.append(spectral_mean)
                features.append(spectral_std)
            except:
                features.extend([0.1, 0.05])  # Fallback values
            
            print(f"Enhanced features extracted: {len(features)}")
            return np.array(features)
            
        except Exception as e:
            print(f"Error extracting enhanced features, falling back to basic: {e}")
            # Fallback to basic features if enhanced fails
            return self.extract_basic_features(audio_file)

    def calculate_similarity_score(self, test_features):
        """Calculate similarity score (uses advanced method if multiple references exist)"""
        if test_features is None:
            return 0.0
        # If multi-reference profile available, prefer advanced calculation
        if hasattr(self, 'reference_samples'):
            return self.calculate_advanced_similarity_score(test_features)

        if getattr(self, 'reference_features', None) is None:
            return 0.0

        if SKLEARN_AVAILABLE:
            # Normalize test features
            test_features_reshaped = test_features.reshape(1, -1)
            test_features_normalized = self.scaler.transform(test_features_reshaped)[0]

            # Cosine similarity
            cosine_sim = cosine_similarity([self.reference_features], [test_features_normalized])[0][0]

            # Euclidean distance
            euclidean_dist = euclidean(self.reference_features, test_features_normalized)
            max_possible_dist = np.sqrt(len(self.reference_features) * 4)
            euclidean_sim = 1 - (euclidean_dist / (max_possible_dist + 1e-8))

            # Correlation
            correlation = np.corrcoef(self.reference_features, test_features_normalized)[0, 1]
            if np.isnan(correlation):
                correlation = 0.0

            combined_score = (0.5 * cosine_sim + 0.3 * euclidean_sim + 0.2 * abs(correlation))
        else:
            # Basic similarity without sklearn
            ref_norm = self.reference_features / (np.linalg.norm(self.reference_features) + 1e-8)
            test_norm = test_features / (np.linalg.norm(test_features) + 1e-8)
            combined_score = np.dot(ref_norm, test_norm)

        return max(0.0, min(1.0, combined_score))  # Clamp between 0 and 1
    
    def train_reference(self, reference_file):
        """Train with reference voice sample"""
        print("Training with reference voice...")
        self.reference_features = self.extract_enhanced_features(reference_file)
        if self.reference_features is not None:
            if SKLEARN_AVAILABLE:
                # Normalize features
                features_reshaped = self.reference_features.reshape(1, -1)
                self.scaler.fit(features_reshaped)
                self.reference_features = self.scaler.transform(features_reshaped)[0]
            print(f"Reference voice trained successfully with {len(self.reference_features)} features")
            return True
        else:
            print("Failed to extract features from reference voice")
            return False
    
    def calculate_advanced_similarity_score(self, test_features):
        """Advanced similarity calculation using multiple reference samples"""
        if not hasattr(self, 'reference_samples') or test_features is None:
            return 0.0
        
        if SKLEARN_AVAILABLE:
            # Normalize test features
            test_features_reshaped = test_features.reshape(1, -1)
            test_features_normalized = self.scaler.transform(test_features_reshaped)[0]
            
            # Calculate similarity to each reference sample
            similarities = []
            for ref_sample in self.reference_samples:
                # Cosine similarity
                cosine_sim = cosine_similarity([ref_sample], [test_features_normalized])[0][0]
                similarities.append(cosine_sim)
            
            # Use the best match (highest similarity)
            best_similarity = np.max(similarities)
            
            # Also check against mean profile
            mean_similarity = cosine_similarity([self.reference_mean], [test_features_normalized])[0][0]
            
            # Weighted combination
            final_score = 0.7 * best_similarity + 0.3 * mean_similarity
            
            # Bonus for consistency (lower std deviation from mean)
            feature_consistency = 1.0 / (1.0 + np.mean(np.abs(test_features_normalized - self.reference_mean) / (self.reference_std + 1e-8)))
            final_score = final_score * (0.8 + 0.2 * feature_consistency)
        
        else:
            # Basic similarity without sklearn
            similarities = []
            test_norm = test_features / (np.linalg.norm(test_features) + 1e-8)
            
            for ref_sample in self.reference_samples:
                ref_norm = ref_sample / (np.linalg.norm(ref_sample) + 1e-8)
                sim = np.dot(ref_norm, test_norm)
                similarities.append(sim)
            
            final_score = np.max(similarities)
        
        return max(0.0, min(1.0, final_score))
    
    def match_voice(self, test_file, threshold=0.6, detailed_output=False):
        """Voice matching with detailed scoring"""
        test_features = self.extract_enhanced_features(test_file)
        if test_features is None:
            print("Failed to extract features from test voice")
            return False
        similarity_score = self.calculate_similarity_score(test_features)
        if detailed_output:
            print(f"Voice similarity score: {similarity_score:.4f}")
            print(f"Threshold: {threshold}")
            print(f"Match: {'YES' if similarity_score >= threshold else 'NO'}")
        return similarity_score >= threshold

# Legacy function for backward compatibility
def extract_mfcc(file):
    """Legacy function - kept for compatibility"""
    try:
        y, sr = librosa.load(file)
        mfcc = librosa.feature.mfcc(y=y, sr=sr, n_mfcc=13)
        return np.mean(mfcc, axis=1)  # Return mean across time
    except:
        return np.zeros(13)

def match_voice(ref_file, test_file, threshold=0.6):
    """Legacy function - kept for compatibility"""
    matcher = EnhancedVoiceMatcher()
    
    if not matcher.train_reference(ref_file):
        return False
    
    return matcher.match_voice(test_file, threshold, detailed_output=True)