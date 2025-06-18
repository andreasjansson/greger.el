#!/usr/bin/env python3
"""
Greger Logo Generator

Generates 20 logo variations for greger.el inspired by the spy character Greger Tragg
from Kjell-Olof Bornemark's novels. Half will include text, half will be text-free.

Based on the character description:
- Greger Tragg is a secret agent in Stockholm
- A communist who has lost faith, works for East German intelligence
- Lives in the shadowy world of espionage during the Cold War era
- A freelance writer who feeds secrets to the East Germans
- Operates in moral gray areas, a "faithless" character
- Set in the world of John le Carré-style spy fiction
"""

import replicate
import asyncio
import aiohttp
import os
from pathlib import Path
import random
from concurrent.futures import ThreadPoolExecutor, as_completed
import time
import requests

# Set up Replicate token - check if it's available in environment
if 'REPLICATE_API_TOKEN' not in os.environ:
    print("❌ REPLICATE_API_TOKEN not found in environment variables")
    print("Please set your Replicate API token:")
    print("export REPLICATE_API_TOKEN=your_token_here")
    exit(1)

# Create output directory
OUTPUT_DIR = Path("./output")
OUTPUT_DIR.mkdir(exist_ok=True)

# Logo prompts - inspired by Greger Tragg's spy character
# Each prompt MUST include a clear spy/agent avatar representation
LOGO_PROMPTS = [
    # Text-based logos (25)
    "Professional logo with 'greger.el' text, featuring a mysterious spy agent silhouette in trench coat and fedora hat, magnifying glass and document symbols, Cold War aesthetic, minimalist design with Swedish blue and yellow colors",
    "Modern logo with 'greger.el' text, stylized secret agent character with briefcase, Stockholm cityscape background, clean vector art style, corporate design with red accent color",
    "Playful logo featuring 'greger.el' text, cartoon spy mascot character with sunglasses and trench coat, friendly and approachable design, colorful retro aesthetic with warm colors",
    "Sophisticated logo with 'greger.el' text, elegant spy agent portrait silhouette, encrypted code symbols, vintage typewriter font elements, film noir black and white with gold accents",
    "Creative logo with 'greger.el' text, spy agent figure against Berlin Wall backdrop, Cold War era design, concrete textures with Swedish flag colors, dramatic lighting",
    "Tech-inspired logo featuring 'greger.el' text, cyberpunk spy agent with digital elements, computer terminals and surveillance themes, retro-futuristic neon green and dark colors",
    "Literary logo with 'greger.el' text, journalist spy character with notepad and pen, espionage gadgets, bookish aesthetic with warm earth tones and ink blue",
    "Bold logo featuring 'greger.el' text, double agent character with split face design, mirror effect showing dual identity, dramatic red and black color scheme",
    "Vintage logo with 'greger.el' text, 1980s spy agent with camera and documents, Stockholm setting, newsprint texture, sepia tones with selective color highlights",
    "Minimalist logo with 'greger.el' text, abstract geometric spy agent figure, Scandinavian design influence, cool blues and grays with clean typography",
    "Art deco logo with 'greger.el' text, stylized spy agent in 1920s style suit, geometric patterns, gold and black color scheme, luxury aesthetic",
    "Noir logo featuring 'greger.el' text, detective spy character with cigarette and hat, shadow play, high contrast black and white with dramatic lighting",
    "Steampunk logo with 'greger.el' text, Victorian era spy agent with brass gadgets, clockwork elements, copper and bronze color palette",
    "Comic book logo with 'greger.el' text, superhero spy character with cape and mask, dynamic pose, bold primary colors, action-packed design",
    "Pixel art logo featuring 'greger.el' text, retro 8-bit spy agent character, video game aesthetic, limited color palette, nostalgic gaming style",
    "Watercolor logo with 'greger.el' text, artistic spy agent silhouette with paint splatters, creative and expressive, soft pastel colors",
    "Grunge logo featuring 'greger.el' text, punk rock spy character with leather jacket, distressed textures, rebellious aesthetic, dark colors",
    "Anime logo with 'greger.el' text, stylized manga spy agent character, Japanese comic book style, vibrant colors, dynamic composition",
    "Retro-futuristic logo with 'greger.el' text, space age spy agent with chrome suit, atomic era design, bright metallic colors",
    "Isometric logo featuring 'greger.el' text, 3D spy agent character in technical drawing style, precise geometric design, modern tech aesthetic",
    "Silhouette logo with 'greger.el' text, dramatic spy agent outline against colorful background, high contrast, bold and striking",
    "Sketch logo with 'greger.el' text, hand-drawn spy agent character, artistic linework, pencil sketch aesthetic, monochromatic with highlights",
    "Constructivist logo featuring 'greger.el' text, Soviet propaganda style spy agent, revolutionary design, red and black bold graphics",
    "Bauhaus logo with 'greger.el' text, modernist spy agent design, geometric shapes, primary colors, clean functional aesthetic",
    "Pop art logo featuring 'greger.el' text, Warhol-style spy agent character, bright colors, repetitive patterns, commercial art aesthetic",
    
    # Text-free logos (25)
    "Stylized spy agent emblem, mysterious figure in trench coat and fedora, magnifying glass and encrypted documents, Swedish flag colors, circular badge design",
    "Secret agent icon, professional spy character with briefcase, combination lock elements, minimalist line art style, monochromatic with accent color",
    "Cartoon spy character mascot, friendly agent with trench coat and sunglasses, playful illustration style, bright and colorful, no text",
    "Abstract spy agent symbol, geometric representation of agent silhouette, surveillance and secrecy themes, modern design with contrasting colors",
    "Cold War spy emblem, agent figure in Stockholm cityscape crosshairs, vintage propaganda poster aesthetic, muted military colors",
    "Tech spy icon, cyberpunk agent silhouette merged with computer chip design, futuristic aesthetic, electric blue and silver colors",
    "Literary spy symbol, agent character with quill pen and book, secret agent tools, academic aesthetic with warm colors",
    "Double agent logo, split spy character face showing dual identity, psychological thriller theme, high contrast black and white with red accent",
    "Retro spy badge, 1980s agent figure with geometric patterns, vintage computing aesthetic, nostalgic color palette",
    "Minimalist agent symbol, simple geometric spy character silhouette, mystery and intelligence themes, Scandinavian design, clean and modern",
    "Art deco spy emblem, elegant agent figure in 1920s style, geometric patterns, gold and black luxury design",
    "Film noir spy icon, detective character with cigarette and shadow, dramatic lighting, high contrast monochrome",
    "Steampunk spy symbol, Victorian agent with brass gadgets, clockwork elements, copper and bronze tones",
    "Comic book spy emblem, superhero agent with cape and mask, dynamic action pose, bold primary colors",
    "Pixel art spy icon, retro 8-bit agent character, video game style, limited color palette, nostalgic design",
    "Watercolor spy symbol, artistic agent silhouette with paint effects, creative expression, soft pastel colors",
    "Grunge spy emblem, punk rock agent with leather jacket, distressed textures, rebellious dark aesthetic",
    "Anime spy icon, manga-style agent character, Japanese comic book aesthetic, vibrant colors, dynamic pose",
    "Retro-futuristic spy symbol, space age agent with chrome elements, atomic era design, bright metallic colors",
    "Isometric spy emblem, 3D agent character in technical style, precise geometric design, modern tech look",
    "Silhouette spy icon, dramatic agent outline against colorful backdrop, high contrast, bold and striking",
    "Sketch spy symbol, hand-drawn agent character, artistic linework, pencil aesthetic, monochrome with highlights",
    "Constructivist spy emblem, Soviet propaganda style agent, revolutionary design, red and black bold graphics",
    "Bauhaus spy icon, modernist agent design, geometric shapes, primary colors, clean functional look",
    "Pop art spy symbol, Warhol-style agent character, bright colors, repetitive patterns, commercial art style"
]

def generate_single_logo(prompt_index, prompt):
    """Generate a single logo using Flux and remove background"""
    try:
        print(f"Starting logo {prompt_index + 1}/50: {'Text' if prompt_index < 25 else 'No text'}")
        
        # Generate image with Flux 1.1 Pro
        print(f"  Generating image for logo {prompt_index + 1}...")
        flux_output = replicate.run(
            "black-forest-labs/flux-1.1-pro",
            input={
                "prompt": prompt,
                "aspect_ratio": "1:1",
                "output_format": "png",
                "output_quality": 95,
                "safety_tolerance": 2,
                "prompt_upsampling": True,
                "seed": random.randint(1, 1000000)
            }
        )
        
        # The flux output is a URL string, need to download it
        # Download the original image with timeout
        original_path = OUTPUT_DIR / f"logo_{prompt_index + 1:02d}_original.png"
        response = requests.get(flux_output, stream=True, timeout=30)
        response.raise_for_status()
        
        with open(original_path, 'wb') as f:
            for chunk in response.iter_content(chunk_size=8192):
                f.write(chunk)
        print(f"  Saved original: {original_path}")
        
        # Remove background - pass the URL directly
        print(f"  Removing background for logo {prompt_index + 1}...")
        bg_removed_output = replicate.run(
            "851-labs/background-remover:a029dff38972b5fda4ec5d75d7d1cd25aeff621d2cf4946a41055d7db66b80bc",
            input={
                "image": flux_output,  # This is a URL string, which the model accepts
                "background_type": "rgba",
                "format": "png",
                "threshold": 0.0
            }
        )
        
        # Download the background-removed image with timeout
        final_path = OUTPUT_DIR / f"logo_{prompt_index + 1:02d}_final.png"
        response = requests.get(bg_removed_output, stream=True, timeout=30)
        response.raise_for_status()
        
        with open(final_path, 'wb') as f:
            for chunk in response.iter_content(chunk_size=8192):
                f.write(chunk)
        print(f"  ✓ Completed logo {prompt_index + 1}: {final_path}")
        
        return prompt_index + 1, True, None
        
    except Exception as e:
        error_msg = f"Error generating logo {prompt_index + 1}: {str(e)}"
        print(f"  ✗ {error_msg}")
        return prompt_index + 1, False, error_msg

def main():
    """Main function to generate all logos in parallel"""
    print("🎨 Starting Greger Logo Generation")
    print("=" * 50)
    print(f"Generating 50 logo variations for greger.el")
    print(f"- 25 with 'greger.el' text")
    print(f"- 25 without text")
    print(f"Theme: Secret agent inspired by Greger Tragg")
    print(f"Output directory: {OUTPUT_DIR.absolute()}")
    print("=" * 50)
    
    start_time = time.time()
    
    # Generate all logos in parallel using ThreadPoolExecutor with maximum parallelism
    with ThreadPoolExecutor(max_workers=20) as executor:
        # Submit all tasks
        future_to_index = {
            executor.submit(generate_single_logo, i, prompt): i 
            for i, prompt in enumerate(LOGO_PROMPTS)
        }
        
        # Collect results as they complete
        completed = 0
        successes = 0
        errors = []
        
        for future in as_completed(future_to_index):
            logo_num, success, error = future.result()
            completed += 1
            
            if success:
                successes += 1
            else:
                errors.append((logo_num, error))
            
            print(f"Progress: {completed}/50 completed ({successes} successful)")
    
    end_time = time.time()
    duration = end_time - start_time
    
    print("=" * 50)
    print("🎉 Logo Generation Complete!")
    print(f"Total time: {duration:.1f} seconds")
    print(f"Successful: {successes}/50")
    print(f"Failed: {len(errors)}/50")
    
    if errors:
        print("\nErrors encountered:")
        for logo_num, error in errors:
            print(f"  Logo {logo_num}: {error}")
    
    print(f"\n📁 Check your logos in: {OUTPUT_DIR.absolute()}")
    print("\nFiles generated:")
    print("- logo_XX_original.png (with background)")
    print("- logo_XX_final.png (transparent background)")
    print("\nLogos 1-25: Include 'greger.el' text")
    print("Logos 26-50: Text-free designs")

if __name__ == "__main__":
    main()
