# 🤖 Jarvis AI - Voice-Controlled Computer Assistant

A sophisticated voice-controlled AI assistant inspired by Tony Stark's Jarvis. This system uses local AI (Ollama) for processing commands and controlling your computer through natural voice interactions, providing a truly intelligent and responsive digital assistant experience.

## ✨ Advanced Features & Capabilities

### 🎤 **Intelligent Voice Recognition System**
- **Multi-Wake Word Support**: Responds to "Jarvis", "Hey Jarvis", "Computer", "Assistant", and custom wake words
- **Continuous Listening Mode**: Always ready to respond with background monitoring
- **Noise Filtering & Audio Optimization**: Advanced signal processing for clear voice detection in noisy environments
- **Dynamic Energy Threshold Adjustment**: Automatically adapts to your environment's audio conditions
- **Real-time Speech Processing**: Instant command recognition and response with minimal latency

### 🧠 **Local AI Brain (Privacy-First Architecture)**
- **Offline Processing**: Complete privacy with local Ollama integration - no data leaves your machine
- **Context-Aware Conversations**: Maintains conversation history and understands follow-up commands
- **Natural Language Understanding**: Interprets complex, conversational commands with high accuracy
- **Command Intent Recognition**: Automatically extracts actionable tasks from natural speech
- **Confidence Scoring**: Evaluates response certainty to provide reliable interactions
- **Personality Engine**: Witty, intelligent responses mimicking Tony Stark's Jarvis character

### 💻 **Comprehensive System Control**
- **Application Lifecycle Management**: Launch, switch, minimize, maximize, and close applications
- **Window Orchestration**: Advanced window positioning and workspace management
- **File System Navigation**: Open folders, navigate directories, and manage files through voice
- **System Resource Monitoring**: Real-time CPU, memory, disk usage, and battery status reporting
- **Process Management**: Monitor and control running applications and system processes
- **Keyboard & Mouse Automation**: Execute complex input sequences via voice commands

### 🌐 **Advanced Web Integration**
- **Intelligent Search Routing**: Automatically determines best search engine (Google, YouTube, Wikipedia) based on query context
- **Multi-Platform Web Control**: Direct browser navigation and tab management
- **Content-Aware Browsing**: Understands search intent and optimizes query formatting
- **Social Media Integration**: Quick access to platforms and content sharing
- **Web Automation**: Form filling, navigation, and interaction through voice commands

### 📱 **Smart Application Ecosystem**
- **Productivity Suite Control**: Seamless integration with IDEs, text editors, and development tools
- **Media Center Management**: Control Spotify, VLC, streaming services, and media playback
- **Communication Hub**: Manage Discord, Slack, email clients, and messaging platforms
- **Creative Tools Integration**: Voice control for design software, image editors, and creative applications
- **Custom Application Profiles**: Personalized command sets for different software environments

### 📊 **Advanced System Analytics**
- **Performance Metrics Dashboard**: Comprehensive system health monitoring via voice queries
- **Resource Usage Predictions**: Intelligent alerts for system resource bottlenecks
- **Network Connectivity Monitoring**: Real-time internet speed, connection quality, and troubleshooting
- **Hardware Status Reporting**: Temperature monitoring, fan speeds, and component health checks
- **Energy Management**: Battery optimization and power consumption analysis

### 📸 **Computer Vision Capabilities**
- **Screenshot Analysis**: Automatic screen content interpretation with LLaVA integration
- **Visual Command Recognition**: Understanding screen context for more accurate responses
- **OCR Integration**: Text extraction from screenshots and images
- **UI Element Detection**: Identifying and interacting with specific interface components
- **Screen Recording Management**: Voice-controlled screen capture and recording functionality

### 🔊 **Advanced Audio Control**
- **Multi-Device Audio Management**: Control system speakers, headphones, and connected audio devices
- **Spatial Audio Adjustment**: Fine-tuned volume control for different applications
- **Audio Profile Switching**: Quick switching between different audio configurations
- **Microphone Management**: Input device selection and sensitivity adjustment
- **Audio Feedback Customization**: Personalized response tones and audio cues

### 🪟 **Intelligent Window Management**
- **Workspace Orchestration**: Multi-monitor setup optimization and window distribution
- **Application Grouping**: Logical organization of related applications across virtual desktops
- **Focus Management**: Smart switching between applications based on current task context
- **Layout Templates**: Predefined window arrangements for different workflows (coding, design, gaming)
- **Distraction-Free Modes**: Automatic hiding of non-essential applications during focus sessions

### ⏰ **Smart Reminder & Task System**
- **Natural Language Scheduling**: "Remind me to call mom in 30 minutes" or "Set up meeting for next Tuesday"
- **Context-Aware Reminders**: Location-based and application-specific notifications
- **Recurring Task Management**: Weekly, daily, and custom interval reminder scheduling
- **Priority-Based Notifications**: Intelligent reminder importance ranking and delivery timing
- **Cross-Session Persistence**: Reminders survive system restarts and application closures

### 🎯 **Adaptive Learning Engine**
- **Usage Pattern Recognition**: Learns your daily routines and proactively suggests optimizations
- **Command Frequency Analysis**: Prioritizes frequently used commands for faster recognition
- **Personalization Layer**: Adapts responses to match your communication style and preferences
- **Error Learning**: Improves accuracy by learning from misunderstood commands
- **Predictive Suggestions**: Anticipates next actions based on current context and historical patterns

## 🎯 Available Commands

The AI understands natural language and can execute various actions:

### System Control
- "Open Chrome" / "Launch Firefox"
- "Take a screenshot"
- "Check system status"
- "Control volume up/down/mute"
- "Minimize/maximize window"

### Web & Search
- "Search Google for Python tutorials"
- "Open YouTube and search for Iron Man scenes"
- "Go to GitHub.com"

### Application Management
- "Switch to VS Code"
- "Open calculator"
- "Launch Spotify"

### Utilities
- "Set reminder for 5 minutes: Call mom"
- "Open Downloads folder"
- "What time is it?"

## 🚀 Setup Instructions

### Prerequisites

1. **Python 3.9+** installed on your system
2. **Ollama** installed and running locally
3. **Microphone** for voice input
4. **Speakers/Headphones** for audio feedback

### Step 1: Install Ollama

```bash
# Install Ollama (visit https://ollama.ai for installation)
# Then pull the required model:
ollama pull mistral
```

### Step 2: Clone and Setup

```bash
git clone https://github.com/yourusername/jarvis-ai.git
cd jarvis-ai

# Create virtual environment
python -m venv venv

# Activate virtual environment
# Windows:
venv\Scripts\activate
# macOS/Linux:
source venv/bin/activate

# Install dependencies
pip install -r requirements.txt
```

### Step 3: Start Ollama Service

```bash
# In a separate terminal, start Ollama
ollama serve
```

### Step 4: Run Jarvis

```bash
python main_jarvis_voice.py
```

## ⚙️ Configuration

### Model Selection
Edit `config.py` to change the AI model:
```python
class AIConfig:
    AI_MODEL = "mistral"  # Options: "mistral", "llama2", "codellama"
```

### Wake Words
Customize wake words in `config.py`:
```python
WAKE_WORDS = [
    "jarvis",
    "hey jarvis", 
    "okay jarvis",
    "computer",
    "assistant"
]
```

### Voice Settings
Adjust TTS settings:
```python
TTS_RATE = 180  # Speech rate (words per minute)
TTS_VOLUME = 0.9
```

## 🏗️ Project Structure

```
jarvis-ai/
├── main_jarvis_voice.py      # Main application entry point
├── ai_brain_local.py         # Local AI processing with Ollama
├── config.py                 # Configuration settings
├── utils.py                  # Utility functions
├── requirements.txt          # Python dependencies
├── README.md                 # Project documentation
└── venv/                     # Virtual environment
```

## 🎛️ Core Components

### TTSManager
- Thread-safe text-to-speech management
- Prevents audio conflicts
- Customizable voice settings

### JarvisLocalAI
- Local AI brain using Ollama
- Command processing and action extraction
- Context-aware conversations
- Screenshot analysis (with LLaVA model)

### Voice Recognition
- Continuous listening mode
- Wake word detection
- Noise filtering and audio optimization

## 🔧 Advanced Features

### Custom Commands
Add custom command sequences in `config.py`:
```python
CUSTOM_COMMANDS = {
    "morning routine": [
        "open chrome",
        "open spotify",
        "what's the weather"
    ]
}
```

### Vision Analysis
Install LLaVA for screenshot analysis:
```bash
ollama pull llava
```

### System Integration
The assistant can:
- Monitor system performance
- Control application windows
- Manage file operations
- Set system reminders

## 🛠️ Troubleshooting

### Common Issues

1. **"Ollama not running"**
   ```bash
   ollama serve
   ```

2. **Microphone not detected**
   - Check system audio settings
   - Verify microphone permissions

3. **TTS initialization failed**
   - Install/update audio drivers
   - Check system audio settings

4. **Import errors**
   - Ensure virtual environment is activated
   - Reinstall requirements: `pip install -r requirements.txt`

### Performance Optimization

- Adjust `AI_TEMPERATURE` for response creativity
- Modify `MAX_CONTEXT_HISTORY` for memory usage
- Set `COMMAND_TIMEOUT` for responsiveness

## 📋 Requirements

### Hardware
- Microphone (USB or built-in)
- Speakers or headphones
- 4GB+ RAM recommended
- Multi-core CPU for better performance

### Software
- Python 3.9 or higher
- Ollama with compatible models
- Operating System: Windows 10+, macOS 10.15+, or Linux

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Commit changes: `git commit -m 'Add amazing feature'`
4. Push to branch: `git push origin feature/amazing-feature`
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Inspired by Tony Stark's Jarvis from the Marvel Cinematic Universe
- Built with [Ollama](https://ollama.ai) for local AI processing
- Uses [SpeechRecognition](https://github.com/Uberi/speech_recognition) for voice input
- Powered by [pyttsx3](https://github.com/nateshmbhat/pyttsx3) for text-to-speech

## 📞 Support

If you encounter issues or have questions:
- Open an issue on GitHub
- Check the troubleshooting section
- Review the configuration options

---

**Note**: This is a local AI assistant that prioritizes privacy by running everything on your machine. No data is sent to external services unless explicitly configured (like web searches).