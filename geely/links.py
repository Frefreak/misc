#!/usr/bin/env python3

import json
import fitz  # PyMuPDF
import os
import sys

def load_url_mapping(mapping_file):
    """Load the URL to page mapping from JSON file."""
    try:
        with open(mapping_file, 'r', encoding='utf-8') as f:
            return json.load(f)
    except FileNotFoundError:
        print(f"Error: {mapping_file} not found. Run merger.py first.")
        sys.exit(1)
    except json.JSONDecodeError:
        print(f"Error: Invalid JSON in {mapping_file}")
        sys.exit(1)

def process_pdf_links(pdf_path, url_mapping, output_path=None):
    """Process PDF to convert external links to internal links."""
    if output_path is None:
        output_path = pdf_path.replace('.pdf', '_with_internal_links.pdf')
    
    print(f"Processing: {pdf_path}")
    print(f"Output: {output_path}")
    
    try:
        # Open the PDF
        doc = fitz.open(pdf_path)
        links_converted = 0
        total_links = 0
        
        # Process each page
        for page_num in range(len(doc)):
            page = doc[page_num]
            
            # Get all links on this page
            links = page.get_links()
            total_links += len(links)
            
            if not links:
                continue
                
            print(f"Page {page_num + 1}: Found {len(links)} links")
            
            # Process each link
            for link in links:
                # Check if it's an external URI link
                if link.get('kind') == fitz.LINK_URI:
                    uri = link.get('uri', '')
                    
                    # Check if this URI matches any of our internal URLs
                    matched_page = None
                    uri_cleaned = uri.strip()
                    
                    # Try different matching strategies
                    for internal_url, target_page in url_mapping.items():
                        # Exact match
                        if uri_cleaned == internal_url:
                            matched_page = target_page
                            break
                        # Prefix match
                        elif uri_cleaned.startswith(internal_url):
                            matched_page = target_page
                            break
                        # Match without anchor fragments
                        elif '#' in uri_cleaned and uri_cleaned.split('#')[0] == internal_url:
                            matched_page = target_page
                            break
                    
                    if matched_page is not None:
                        print(f"  Converting: {uri} → page {matched_page + 1}")
                        
                        # Create new internal link
                        new_link = {
                            'kind': fitz.LINK_GOTO,
                            'from': link['from'],  # Same clickable area
                            'page': matched_page,   # Target page (0-based)
                            'to': fitz.Point(0, 0)  # Top-left of target page
                        }
                        
                        # Remove the old link
                        page.delete_link(link)
                        
                        # Add the new internal link
                        page.insert_link(new_link)
                        
                        links_converted += 1
        
        print(f"\nSummary:")
        print(f"  Total links found: {total_links}")
        print(f"  External links converted to internal: {links_converted}")
        print(f"  External links remaining: {total_links - links_converted}")
        
        # Save the modified PDF
        doc.save(output_path)
        doc.close()
        
        print(f"\nProcessed PDF saved as: {output_path}")
        return links_converted
        
    except Exception as e:
        print(f"Error processing PDF: {e}")
        import traceback
        traceback.print_exc()
        return 0

def main():
    # File paths
    pdf_file = 'merged_manual.pdf'
    mapping_file = 'url_to_page_mapping.json'
    output_file = 'merged_manual_with_internal_links.pdf'
    
    # Check if files exist
    if not os.path.exists(pdf_file):
        print(f"Error: {pdf_file} not found. Run merger.py first.")
        sys.exit(1)
    
    if not os.path.exists(mapping_file):
        print(f"Error: {mapping_file} not found. Run merger.py first.")
        sys.exit(1)
    
    print("🔗 PDF Link Processor")
    print("=" * 40)
    
    # Load the URL mapping
    print("Loading URL mapping...")
    url_mapping = load_url_mapping(mapping_file)
    print(f"Loaded {len(url_mapping)} URL mappings")
    
    # Show sample mappings
    print("\nSample URL mappings:")
    for i, (url, page) in enumerate(list(url_mapping.items())[:3]):
        print(f"  {url} → page {page + 1}")
    if len(url_mapping) > 3:
        print("  ...")
    
    # Process the PDF
    print("\nProcessing PDF links...")
    links_converted = process_pdf_links(pdf_file, url_mapping, output_file)
    
    if links_converted > 0:
        print(f"\n✅ Success! Converted {links_converted} external links to internal links.")
        print(f"📄 Final PDF: {output_file}")
        
        # Optional: Replace the original file
        replace_original = input("\nReplace original merged_manual.pdf? (y/N): ").lower().strip()
        if replace_original == 'y':
            os.replace(output_file, pdf_file)
            print(f"✅ Original file replaced: {pdf_file}")
        else:
            print(f"📄 Original file preserved: {pdf_file}")
            print(f"📄 New file with internal links: {output_file}")
    else:
        print("❌ No links were converted.")
    
    print("\nDone! 🎉")

if __name__ == '__main__':
    main()
