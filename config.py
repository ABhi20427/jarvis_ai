"""
config.py - Configuration file for Jarvis
Place this file in the same directory as enhanced_jarvis.py
This file contains all configurable settings
"""

import os
from pathlib import Path

# Base Configuration
class Config:
    # Audio Settings
    SAMPLE_RATE = 44100
    CHANNELS = 1
    RECORDING_DURATION = 5
    CHUNK_SIZE = 1024
    
    # Speech Recognition
    ENERGY_THRESHOLD = 4000
    DYNAMIC_ENERGY = True
    PAUSE_THRESHOLD = 0.8
    PHRASE_THRESHOLD = 0.3
    NON_SPEAKING_DURATION = 0.5
    
    # Text-to-Speech
    TTS_RATE = 180  # Speech rate (words per minute)
    TTS_VOLUME = 0.9
    TTS_VOICE = 0  # Voice index (0 for default)
    
    # Wake Words
    WAKE_WORDS = [
        "jarvis",
        "hey jarvis", 
        "okay jarvis",
        "computer",
        "assistant",
        "friday"  # Alternative assistant name
    ]
    
    # Paths
    HOME_DIR = Path.home()
    DOCUMENTS_DIR = HOME_DIR / "Documents"
    DOWNLOADS_DIR = HOME_DIR / "Downloads"
    PICTURES_DIR = HOME_DIR / "Pictures"
    JARVIS_DIR = HOME_DIR / ".jarvis"  # Hidden directory for Jarvis data
    
    # Create Jarvis directory if it doesn't exist
    JARVIS_DIR.mkdir(exist_ok=True)
    
    SCREENSHOT_DIR = PICTURES_DIR / "Screenshots"
    SCREENSHOT_DIR.mkdir(exist_ok=True)
    
    LOG_FILE = JARVIS_DIR / "jarvis.log"
    COMMANDS_HISTORY = JARVIS_DIR / "commands_history.json"
    USER_PREFERENCES = JARVIS_DIR / "preferences.json"
    
    # API Keys (Add your own keys here)
    OPENWEATHER_API_KEY = ""  # Get from openweathermap.org
    NEWS_API_KEY = ""  # Get from newsapi.org
    WOLFRAM_API_KEY = ""  # Get from wolframalpha.com
    
    # Feature Flags
    ENABLE_WAKE_WORD = True
    ENABLE_VOICE_FEEDBACK = True
    ENABLE_LOGGING = True
    ENABLE_COMMAND_HISTORY = True
    ENABLE_WEB_SEARCH = True
    ENABLE_SMART_HOME = False  # Set to True if you have smart home devices
    
    # Performance Settings
    MAX_HISTORY_SIZE = 100
    COMMAND_TIMEOUT = 10  # seconds
    LISTENING_TIMEOUT = 5  # seconds
    
    # Web URLs
    SEARCH_ENGINE = "https://www.google.com/search?q="
    YOUTUBE_SEARCH = "https://www.youtube.com/results?search_query="
    WIKIPEDIA_SEARCH = "https://en.wikipedia.org/wiki/"
    
    # Application Paths (Windows)
    # Modify these based on your system
    APP_PATHS = {
        'chrome': 'chrome',
        'firefox': 'firefox',
        'edge': 'msedge',
        'notepad': 'notepad',
        'calculator': 'calc',
        'vscode': 'code',
        'spotify': 'spotify',
        'discord': 'discord',
        'slack': 'slack',
        'terminal': 'cmd',
        'powershell': 'powershell',
        'explorer': 'explorer'
    }
    
    # Custom Commands
    # Add your own custom commands here
    CUSTOM_COMMANDS = {
        "morning routine": [
            "open chrome",
            "open spotify",
            "what's the weather",
            "what's the news"
        ],
        "work mode": [
            "open vscode",
            "open slack",
            "open chrome"
        ],
        "gaming mode": [
            "open discord",
            "open steam"
        ]
    }
    
    # Response Variations
    RESPONSES = {
        'startup': [
            "Systems initialized. At your service.",
            "All systems operational. Ready for commands.",
            "Initialization complete. How may I assist you?",
            "Online and ready. What can I do for you today?"
        ],
        'shutdown': [
            "Shutting down. Have a great day!",
            "Going offline. Until next time!",
            "Systems powering down. Goodbye!",
            "Farewell. It's been a pleasure serving you."
        ],
        'error': [
            "I encountered an issue. Shall I try again?",
            "Something went wrong. Let me investigate.",
            "There seems to be a problem with that request.",
            "I'm having difficulty with that command."
        ],
        'success': [
            "Task completed successfully.",
            "Done!",
            "Completed as requested.",
            "All set!"
        ]
    }
    
    # Personality Settings
    PERSONALITY = {
        'name': 'Jarvis',
        'formal': True,  # Formal vs casual speech
        'humor': True,   # Enable humor in responses
        'verbose': False  # Detailed vs concise responses
    }
    
    # Security Settings
    REQUIRE_CONFIRMATION = {
        'shutdown': True,
        'restart': True,
        'delete': True,
        'format': True
    }
    
    # Logging Settings
    LOG_LEVEL = 'INFO'  # DEBUG, INFO, WARNING, ERROR
    LOG_FORMAT = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    
    @classmethod
    def load_user_preferences(cls):
        """Load user preferences from file"""
        import json
        if cls.USER_PREFERENCES.exists():
            try:
                with open(cls.USER_PREFERENCES, 'r') as f:
                    prefs = json.load(f)
                    for key, value in prefs.items():
                        if hasattr(cls, key):
                            setattr(cls, key, value)
            except Exception as e:
                print(f"Error loading preferences: {e}")
    
    @classmethod
    def save_user_preferences(cls, preferences):
        """Save user preferences to file"""
        import json
        try:
            with open(cls.USER_PREFERENCES, 'w') as f:
                json.dump(preferences, f, indent=4)
        except Exception as e:
            print(f"Error saving preferences: {e}")