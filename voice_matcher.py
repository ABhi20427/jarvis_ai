import librosa
import numpy as np
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
            print(f"MFCC features extracted: {len(mfcc_mean + mfcc_std)}")
            
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
    
    def train_reference(self, reference_file):
        """Train with reference voice sample"""
        print("Training with reference voice...")
        self.reference_features = self.extract_basic_features(reference_file)
        
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
    
    def calculate_similarity_score(self, test_features):
        """Calculate similarity score using available methods"""
        if self.reference_features is None or test_features is None:
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
            euclidean_sim = 1 - (euclidean_dist / max_possible_dist)
            
            # Correlation
            correlation = np.corrcoef(self.reference_features, test_features_normalized)[0, 1]
            if np.isnan(correlation):
                correlation = 0.0
            
            combined_score = (0.5 * cosine_sim + 0.3 * euclidean_sim + 0.2 * abs(correlation))
        else:
            # Basic similarity without sklearn
            # Normalize both feature vectors
            ref_norm = self.reference_features / (np.linalg.norm(self.reference_features) + 1e-8)
            test_norm = test_features / (np.linalg.norm(test_features) + 1e-8)
            
            # Dot product similarity (cosine similarity)
            combined_score = np.dot(ref_norm, test_norm)
        
        return max(0.0, min(1.0, combined_score))  # Clamp between 0 and 1
    
    def match_voice(self, test_file, threshold=0.6, detailed_output=False):
        """Voice matching with detailed scoring"""
        test_features = self.extract_basic_features(test_file)
        
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