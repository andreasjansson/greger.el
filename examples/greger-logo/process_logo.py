#!/usr/bin/env python3
"""
Process logo 15 to extract text and logo image separately
"""

from PIL import Image
import numpy as np
from pathlib import Path

def trim_transparent(image):
    """Trim transparent borders from an image"""
    # Convert to numpy array
    img_array = np.array(image)
    
    # Get alpha channel (transparency)
    if img_array.shape[2] == 4:  # RGBA
        alpha = img_array[:, :, 3]
    else:
        # If no alpha channel, assume all pixels are opaque
        return image
    
    # Find non-transparent pixels
    non_transparent = alpha > 0
    
    # Find bounding box
    rows = np.any(non_transparent, axis=1)
    cols = np.any(non_transparent, axis=0)
    
    if not np.any(rows) or not np.any(cols):
        # Image is completely transparent
        return image
    
    row_min, row_max = np.where(rows)[0][[0, -1]]
    col_min, col_max = np.where(cols)[0][[0, -1]]
    
    # Crop the image
    return image.crop((col_min, row_min, col_max + 1, row_max + 1))

def main():
    # Load the logo
    logo_path = Path("./output/logo_15_final.png")
    if not logo_path.exists():
        print(f"❌ Logo file not found: {logo_path}")
        return
    
    image = Image.open(logo_path)
    print(f"Original image size: {image.size}")
    
    # Crop the text region (around 50x330 to 950x530)
    text_region = image.crop((50, 330, 950, 530))
    print(f"Text region size: {text_region.size}")
    
    # Crop the logo image (everything below the text, let's assume it starts around y=530)
    # We'll crop a generous area and then trim
    logo_region = image.crop((0, 530, image.width, image.height))
    print(f"Logo region size before trim: {logo_region.size}")
    
    # Trim transparent borders from both
    text_trimmed = trim_transparent(text_region)
    logo_trimmed = trim_transparent(logo_region)
    
    print(f"Text after trim: {text_trimmed.size}")
    print(f"Logo after trim: {logo_trimmed.size}")
    
    # Create assets directory
    assets_dir = Path("~/projects/greger.el/assets").expanduser()
    assets_dir.mkdir(exist_ok=True)
    
    # Save the processed images
    text_path = assets_dir / "greger-text-logo.png"
    logo_path = assets_dir / "greger-icon-logo.png"
    
    text_trimmed.save(text_path)
    logo_trimmed.save(logo_path)
    
    print(f"✅ Saved text logo: {text_path}")
    print(f"✅ Saved icon logo: {logo_path}")
    
    # Show info about the saved files
    print(f"\nText logo dimensions: {text_trimmed.size}")
    print(f"Icon logo dimensions: {logo_trimmed.size}")

if __name__ == "__main__":
    main()
