"""
ai_brain_local.py - Local AI Brain for Jarvis using Ollama
This file provides AI capabilities without any API costs
Place in same directory as main_jarvis_voice.py
"""

import requests
import json
import re
from typing import Dict, List, Optional, Tuple
from datetime import datetime
import logging

class JarvisLocalAI:
    def __init__(self, model="mistral"):
        """Initialize the local AI brain with Ollama"""
        self.model = model
        self.base_url = "http://localhost:11434"
        self.context_history = []
        self.max_context = 5  # Keep last 5 exchanges
        
        # Test connection
        self.is_available = self.test_connection()
        
        # Jarvis personality and capabilities
        self.system_prompt = """You are Jarvis, an advanced AI assistant like Tony Stark's Jarvis. 
You control a computer through voice commands. You are witty, intelligent, and helpful.

When the user asks you to do something on the computer, structure your response like this:
1. First, give a natural conversational response
2. If action is needed, add "EXECUTE:" followed by the action type and parameters

Available actions you can execute:
- EXECUTE: OPEN_APP [app_name] - Opens applications
- EXECUTE: SEARCH_WEB [query] - Searches Google
- EXECUTE: SEARCH_YOUTUBE [query] - Searches YouTube  
- EXECUTE: OPEN_WEBSITE [url] - Opens specific website
- EXECUTE: TYPE_TEXT [text] - Types text
- EXECUTE: TAKE_SCREENSHOT - Takes screenshot
- EXECUTE: SYSTEM_INFO [type] - Gets system info (battery/cpu/memory)
- EXECUTE: SET_REMINDER [time] [message] - Sets reminder
- EXECUTE: OPEN_FOLDER [folder_name] - Opens folder
- EXECUTE: CONTROL_VOLUME [up/down/mute] - Volume control
- EXECUTE: WINDOW_CONTROL [minimize/maximize/close] - Window control
- EXECUTE: SWITCH_APP [app_name] - Switches to running app

Example response:
User: "Open YouTube and search for Iron Man scenes"
Jarvis: "Certainly sir, opening YouTube with Iron Man scenes for you.
EXECUTE: SEARCH_YOUTUBE Iron Man best scenes"

Be conversational but concise. Add personality to your responses."""

    def test_connection(self) -> bool:
        """Test if Ollama is running"""
        try:
            response = requests.get(f"{self.base_url}/api/tags", timeout=2)
            if response.status_code == 200:
                models = response.json().get('models', [])
                print(f"✅ Ollama connected. Available models: {[m['name'] for m in models]}")
                return True
        except:
            print("⚠️ Ollama not running. Start with: ollama serve")
            return False
    
    def process_command(self, command: str, include_context: bool = True) -> Dict:
        """Process user command with AI understanding"""
        
        if not self.is_available:
            return {
                "success": False,
                "response": "AI is offline. Please start Ollama.",
                "actions": []
            }
        
        # Build conversation context
        context = ""
        if include_context and self.context_history:
            context = "\n".join([
                f"User: {h['user']}\nJarvis: {h['assistant']}" 
                for h in self.context_history[-3:]
            ])
            context = f"Previous conversation:\n{context}\n\n"
        
        # Create full prompt
        full_prompt = f"""{self.system_prompt}

{context}Current command:
User: {command}
Jarvis:"""

        try:
            # Call Ollama API
            response = requests.post(
                f"{self.base_url}/api/generate",
                json={
                    "model": self.model,
                    "prompt": full_prompt,
                    "stream": False,
                    "options": {
                        "temperature": 0.7,
                        "top_p": 0.9,
                    }
                },
                timeout=30
            )
            
            if response.status_code == 200:
                ai_response = response.json()['response']
                
                # Extract actions from response
                actions = self.extract_actions(ai_response)
                
                # Clean response for speech (remove EXECUTE commands)
                clean_response = self.clean_response_for_speech(ai_response)
                
                # Update context
                self.update_context(command, clean_response)
                
                return {
                    "success": True,
                    "response": clean_response,
                    "actions": actions,
                    "raw_response": ai_response
                }
            else:
                return {
                    "success": False,
                    "response": "I'm having trouble processing that.",
                    "actions": []
                }
                
        except Exception as e:
            logging.error(f"AI processing error: {e}")
            return {
                "success": False,
                "response": "I encountered an error processing your request.",
                "actions": []
            }
    
    def extract_actions(self, text: str) -> List[Dict]:
        """Extract executable actions from AI response"""
        actions = []
        
        # Find all EXECUTE commands
        execute_pattern = r'EXECUTE:\s*([A-Z_]+)\s*(.*?)(?=EXECUTE:|$)'
        matches = re.findall(execute_pattern, text, re.IGNORECASE)
        
        for action_type, params in matches:
            action_type = action_type.strip().upper()
            params = params.strip()
            
            action = {"type": action_type, "params": params}
            
            # Parse specific action types
            if action_type == "OPEN_APP":
                action["app"] = params.lower()
            elif action_type in ["SEARCH_WEB", "SEARCH_YOUTUBE"]:
                action["query"] = params
            elif action_type == "OPEN_WEBSITE":
                action["url"] = params if params.startswith("http") else f"https://{params}"
            elif action_type == "TYPE_TEXT":
                action["text"] = params
            elif action_type == "SET_REMINDER":
                parts = params.split(" ", 1)
                if len(parts) == 2:
                    action["time"] = parts[0]
                    action["message"] = parts[1]
            elif action_type == "OPEN_FOLDER":
                action["folder"] = params.lower()
            elif action_type == "CONTROL_VOLUME":
                action["direction"] = params.lower()
            elif action_type == "SWITCH_APP":
                action["app"] = params.lower()
            elif action_type == "WINDOW_CONTROL":
                action["operation"] = params.lower()
            
            actions.append(action)
        
        return actions
    
    def clean_response_for_speech(self, response: str) -> str:
        """Remove EXECUTE commands from response for cleaner speech"""
        # Remove EXECUTE lines
        clean = re.sub(r'EXECUTE:.*?(?=\n|$)', '', response)
        # Clean up extra whitespace
        clean = re.sub(r'\n+', ' ', clean).strip()
        return clean
    
    def update_context(self, user_input: str, assistant_response: str):
        """Update conversation context"""
        self.context_history.append({
            "user": user_input,
            "assistant": assistant_response,
            "timestamp": datetime.now().isoformat()
        })
        
        # Keep only recent context
        if len(self.context_history) > self.max_context:
            self.context_history = self.context_history[-self.max_context:]
    
    def get_suggestions(self, context: str) -> List[str]:
        """Get proactive suggestions based on context"""
        prompt = f"""Based on this context: {context}
        Suggest 3 helpful actions the user might want to do next.
        Format: Just list the suggestions, one per line."""
        
        try:
            response = requests.post(
                f"{self.base_url}/api/generate",
                json={"model": self.model, "prompt": prompt, "stream": False},
                timeout=10
            )
            
            if response.status_code == 200:
                suggestions = response.json()['response'].strip().split('\n')
                return [s.strip() for s in suggestions if s.strip()][:3]
        except:
            pass
        
        return []
    
    def analyze_screen(self, screenshot_path: str) -> str:
        """Analyze screenshot using vision model (if available)"""
        # This would require LLaVA model: ollama pull llava
        try:
            with open(screenshot_path, 'rb') as f:
                image_data = f.read().hex()
            
            response = requests.post(
                f"{self.base_url}/api/generate",
                json={
                    "model": "llava",
                    "prompt": "Describe what you see in this screenshot",
                    "images": [image_data]
                },
                timeout=30
            )
            
            if response.status_code == 200:
                return response.json()['response']
        except:
            return "Vision analysis not available. Install LLaVA model."
        
        return "Could not analyze screenshot"