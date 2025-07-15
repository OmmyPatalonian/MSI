# Histology DESI Overlay Shiny Application

This R Shiny application allows you to overlay histology images with mass spectrometry imaging (MSI) data for registration and analysis.

## Latest Improvements (Full Image Visibility Fix)

**FIXED: Histology Overlay Size Issue**
- **Problem**: Histology images appeared huge and cut off, requiring manual zoom-out
- **Solution**: Fixed overlay sizing logic to use proper proportional units instead of pixel dimensions
- **Result**: Histology images now display at an appropriate 60% default scale, showing the entire image

### Image Size and Overlay Alignment
The MSI data and histology overlay need to be similar sizes for proper alignment:

**Default Settings**: The app now uses balanced default sizes (1200x900px) with standard 4:3 proportions for optimal viewing.

**NEW: MSI Image Quality Improvements**:
- **Balanced Quality Rendering**: Optimized DPI (200) for sharp MSI images with reasonable file sizes
- **Bicubic Interpolation**: Smooths pixelated MSI appearance using advanced interpolation
- **Gaussian Smoothing**: Applied by default to reduce pixelation and improve visual quality
- **Enhanced Contrast**: Histogram and adaptive contrast options for better visibility
- **Optimized Histology Display**: Histology overlay defaults to 60% scale for full image visibility while maintaining alignment

**Size Adjustment Options**:
- **Manual adjustment**: Use the "MSI plot width/height" controls
- **Quick presets**: Use size buttons (Small, Medium, Large, X-Large) with standard 4:3 proportions:
  - Small: 800x600px
  - Medium: 1200x900px (default)
  - Large: 1600x1200px  
  - X-Large: 2000x1500px
- **Scale multiplier**: Use the "Multiplier for images" slider in the sidebar

**Recommended Workflow**:
1. Load both MSI and histology data
2. Default **Medium (1200x900)** size provides balanced viewing of both images
3. **MSI images will be sharp and clear** with good resolution
4. **Histology image will show at 60% scale** for proper full-image visibility
5. Try **Large (1600x1200)** preset if you need more detail
6. Adjust scale multiplier (try 1.0-2.0x) if images need size matching
7. Fine-tune alignment using the transformation controls

**For Large Files**: Use smaller sizes if performance becomes an issue

### File Upload Requirementsass spectrometry imaging (MSI) data for registration and analysis.

## Prerequisites

### Required Software
- **R** (version 4.0 or higher)
- **RStudio** (optional but recommended)

### Required R Packages
The following packages need to be installed:
```r
install.packages("shiny")
install.packages("png")
install.packages("jpeg")
install.packages("tiff")  # For TIFF file support
install.packages("abind") # For array operations
install.packages("ggplot2")
install.packages("grid")

# For Cardinal (Bioconductor package)
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("Cardinal")
```

## Required Files
Make sure you have these files in your working directory:
- `Histology_DESI_overlay (1).R` - Main Shiny application
- `plot_Card_overlay_NEW (1).R` - Required source file with plotting functions

## Running the Application

### Method 1: Using PowerShell (Recommended)
1. Open PowerShell
2. Navigate to the application directory:
   ```powershell
   cd "g:\r script (registration)"
   ```
3. Run the application using the full R path:
   ```powershell
   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "Histology_DESI_overlay (1).R"
   ```

### Method 2: Using RStudio
1. Open RStudio
2. Open the file `Histology_DESI_overlay (1).R`
3. Click "Run App" button or press Ctrl+Shift+Enter

### Method 3: Using R Console
1. Open R console
2. Set working directory:
   ```r
   setwd("g:/r script (registration)")
   ```
3. Run the application:
   ```r
   source("Histology_DESI_overlay (1).R")
   ```

## Application Features

### File Uploads
- **Histology Images**: Upload PNG, JPEG, or TIFF format images
- **MSI Dataset**: Upload .imzML file (and .ibd file if separate)

### Image Transformation Controls
- **Scale**: Adjust X and Y scaling independently (0.1-5x)
- **Rotation**: Rotate image (-180° to +180°)
- **Translation**: Move image in X and Y directions
- **Transparency**: Adjust image transparency (0-1)

### Quick Transform Buttons
Physical buttons for faster transformations:
- **Translation Buttons**: Arrow buttons (↑↓←→) to move image in 1-unit steps
- **Center Button**: (⌂) to reset translation to center
- **Rotation Buttons**: 
  - Coarse rotation: ±5° steps
  - Fine rotation: ±1° steps
  - Reset rotation to 0°
- **Scaling Buttons**: 
  - Coarse scaling: ±0.1 steps
  - Fine scaling: ±0.01 steps
  - Reset scale to 0.6 (default for full image visibility)
- **Reset All**: Red button to reset all transformations to default

### Keyboard Shortcuts (Arrow Keys)
For faster rigid transformations, you can use keyboard shortcuts:
- **Arrow Keys**: Move image (translate)
- **Shift + Left/Right Arrow**: Rotate image counter-clockwise/clockwise
- **Shift + Up/Down Arrow**: Scale Y axis up/down
- **Ctrl + Left/Right Arrow**: Scale X axis down/up
- **Ctrl + Up/Down Arrow**: Scale both axes up/down

*Note: Keyboard shortcuts only work when not focused on input fields*

### Settings Management
- **Save Settings**: Download current transformation settings as RDS file
- **Load Settings**: Upload previously saved settings
- **Restore Settings**: Reset to default values

### Display Options
- **Spatial Plot Only**: Toggle spatial-only view
- **Debug Mode**: Enable debugging visualization
- **Scale Box**: Overall multiplier for image display

## Expected Output
When successfully started, you should see:
```
Listening on http://127.0.0.1:5515
```

Open your web browser and navigate to `http://127.0.0.1:5515` to access the application interface.

## File Upload Requirements

### imzML Files
ImzML files may consist of one or two parts:
- `.imzML` file (contains metadata and possibly all data)  
- `.ibd` file (contains binary data, if separate from .imzML)

**To upload:**

*Option 1: Single .imzML file (if it contains all data)*
1. Click "Upload msi dataset" 
2. Select your .imzML file
3. Click Open

*Option 2: Separate .imzML and .ibd files*
1. Click "Upload msi dataset"
2. Hold Ctrl and click both the .imzML and .ibd files
3. Click Open

Both files must have the same base name (e.g., `data.imzML` and `data.ibd`)

## Troubleshooting

### Missing .ibd File + Unsupported File Type Error
If you get "Missing .ibd file" followed by "Unsupported file type" error:
1. **This is usually caused by conflicting file processing**: The app may be trying to process files twice
2. **Solution steps**:
   - Restart the Shiny app completely
   - Upload files in this exact order: First upload the histology image, then upload MSI files
   - Ensure both .imzML and .ibd files have identical base names (e.g., `sample.imzML` and `sample.ibd`)
   - Use the debug mode to see detailed file processing information
   - Check that file paths don't contain special characters or spaces

### Missing .ibd File Error
If you get "The system cannot find the file specified" error:
1. Your .imzML file may require a separate .ibd file
2. Ensure you upload both .imzML and .ibd files together
3. Verify both files have the same base name
4. Try re-uploading both files together

### R Not Found Error
If you get "R is not recognized" error:
1. Find your R installation path (usually `C:\Program Files\R\R-x.x.x\bin\`)
2. Use the full path to Rscript.exe in the command
3. Alternatively, add R to your system PATH

### Missing Packages Error
If you get package loading errors:
1. Install missing packages using the commands in the Prerequisites section
2. Restart R/RStudio after installation

### Unsupported File Type Error
If you get "Unsupported file type" error:
1. **For histology images**: Ensure your file is PNG, JPG, JPEG, TIFF, or TIF format
2. **Check file extension**: File must have the correct extension (.png, .jpg, .jpeg, .tiff, .tif)
3. **Verify file integrity**: Try opening the image in another program to ensure it's not corrupted
4. **Enable debug mode**: Check "Debug Mode" checkbox to see detailed file information
5. **Single file upload**: Ensure only one histology image is uploaded at a time

### Memory Limit Error ("vector memory exhausted" or "memory limit reached")
If you get a memory limit error when processing large TIFF files:

**Root Cause**: Large TIFF files can exceed R's default memory limits (2GB vector size limit).

**Immediate Solutions**:
1. **Restart R and increase memory limit** (Windows):
   ```r
   memory.limit(size = 8192)  # Set to 8GB
   ```

2. **Close other applications** to free system memory

3. **Pre-convert TIFF to PNG** to reduce file size:
   - ImageJ: File → Export → PNG
   - GIMP: File → Export As → PNG
   - Online converters for smaller files

**Automatic Optimizations Applied**:
- **Memory-efficient TIFF processing**: Only loads first page, frees memory immediately
- **Garbage collection**: Forces memory cleanup during processing  
- **In-place operations**: Reduces temporary object creation
- **Progressive memory freeing**: Releases memory as soon as data is processed

**Prevention Tips**:
- **Reduce TIFF resolution** before upload if possible
- **Use PNG format** instead of TIFF when possible
- **Close other R sessions** before processing large files
- **Restart R** before processing very large images

**Technical Details**: The app now includes automatic memory management with `gc()` calls, progressive memory freeing, and optimized array operations to minimize memory usage during TIFF processing.

### TIFF Image Display Issues (Grey Lines/Artifacts)
If your TIFF histology image shows grey lines or appears split/fragmented:

**Root Cause**: Grey lines are caused by the R graphics engine rendering raster data with gaps between tiles, alpha channel transparency, and inconsistent background handling.

**AUTOMATIC SOLUTION IMPLEMENTED**: The app now automatically converts TIFF files to PNG format during processing to eliminate grey lines entirely:
- **TIFF-to-PNG conversion**: All TIFF files are automatically converted to PNG format in memory before rendering
- **Multi-method TIFF reading**: Tries multiple TIFF reading approaches (all pages, standard, native) to handle different TIFF formats
- **Preserved quality**: Conversion maintains full image quality and supports all TIFF variants (multi-page, tiled, compressed)
- **Seamless overlay**: Converted PNG images render cleanly with MSI data without visual artifacts
- **Standard transparency**: Uses normal RGBA transparency instead of problematic alpha blending

**What happens automatically**:
1. TIFF file is detected during upload
2. Multiple reading methods attempt to load the TIFF data
3. Image is converted to PNG format in memory using `png::writePNG()`
4. PNG version is used for all rendering operations
5. Standard grid graphics handle the PNG overlay without grey lines

**If Issues Still Persist**:
1. **Enable Debug Mode**: Check "Debug Mode" to see TIFF conversion process details
2. **Check R Console**: Look for messages about TIFF conversion and PNG creation
3. **Manual conversion**: Pre-convert TIFF to PNG if automatic conversion fails:
   - ImageJ: File → Export → PNG
   - GIMP: File → Export As → PNG  
4. **Verify TIFF integrity**: Ensure the original TIFF can be opened in other image viewers

**Technical Details**: This solution eliminates grey lines by bypassing the problematic raster rendering of TIFF data entirely. TIFF files are converted to PNG format using R's native PNG handling, which has proven reliable for grid graphics rendering. The conversion preserves all image data while ensuring compatibility with the overlay system.

### File Path Issues
- Ensure all file paths use forward slashes (/) or double backslashes (\\)
- Verify that `plot_Card_overlay_NEW (1).R` exists in the specified path
- Check that the working directory is set correctly

## File Size Limits
- Maximum upload size is set to 500MB
- Adjust `options(shiny.maxRequestSize=500*1024^2)` if needed

## Support
For issues or questions, check:
1. R console output for error messages
2. Shiny application logs in the terminal
3. Verify all required files are present and accessible
