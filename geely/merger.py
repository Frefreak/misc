import os
import json
from PyPDF2 import PdfMerger, PdfReader

FOLDER = './data/'  # Your root folder
MANUAL_JSON = os.path.join(os.path.dirname(__file__), 'manual.json')
BASE_URL = 'https://gtis.geely.com/gum/manual/html/KX11-KX11A3/topics/'

# Helper to build the PDF path from label hierarchy
def get_pdf_path(label_path):
    safe_labels = [label.replace('/', '_') for label in label_path]
    dir_path = os.path.join(FOLDER, *safe_labels[:-1])
    pdf_path = os.path.join(dir_path, f"{safe_labels[-1]}.pdf")
    return pdf_path

# Build a mapping of code -> page number in the final merged PDF
def build_url_to_page_mapping(manual):
    """Build a mapping of URLs to page numbers in the merged PDF."""
    url_to_page = {}
    
    def traverse_for_mapping(node, label_path, current_page):
        label = node.get('label', None)
        if label:
            label_path = label_path + [label]
        
        if 'code' in node:
            code = node['code']
            url = f"{BASE_URL}{code}.html"
            pdf_path = get_pdf_path(label_path)
            
            if os.path.exists(pdf_path):
                try:
                    reader = PdfReader(pdf_path)
                    num_pages = len(reader.pages)
                    url_to_page[url] = current_page
                    # Also map without .html extension
                    url_to_page[f"{BASE_URL}{code}"] = current_page
                    current_page += num_pages
                except Exception as e:
                    print(f"Error reading {pdf_path}: {e}")
        
        if 'children' in node:
            for child in node['children']:
                current_page = traverse_for_mapping(child, label_path, current_page)
        
        return current_page
    
    current_page = 0
    for top in manual['catalog']:
        current_page = traverse_for_mapping(top, [], current_page)
    
    return url_to_page

# Recursively add PDFs and bookmarks
# Returns the number of pages added so far (for correct bookmark placement)
def add_pdfs_and_bookmarks(merger, node, label_path, parent=None, page_offset=0):
    label = node.get('label', None)
    if label:
        label_path = label_path + [label]
    bookmark = None
    start_page = page_offset
    if 'code' in node:
        pdf_path = get_pdf_path(label_path)
        if os.path.exists(pdf_path):
            # Get number of pages before appending
            try:
                reader = PdfReader(pdf_path)
                num_pages = len(reader.pages)
            except Exception as e:
                print(f"Error reading {pdf_path}: {e}")
                num_pages = 0
            merger.append(pdf_path)
            bookmark = merger.add_outline_item(label, start_page, parent=parent)
            page_offset += num_pages
        else:
            print(f"Warning: PDF not found: {pdf_path}")
    elif 'children' in node:
        # Parent bookmark (folder)
        bookmark = merger.add_outline_item(label, start_page, parent=parent)
        for child in node['children']:
            page_offset = add_pdfs_and_bookmarks(merger, child, label_path, parent=bookmark, page_offset=page_offset)
    return page_offset

def main():
    with open(MANUAL_JSON, 'r', encoding='utf-8') as f:
        manual = json.load(f)
    
    print("Step 1: Building URL to page mapping...")
    url_to_page = build_url_to_page_mapping(manual)
    print(f"Found {len(url_to_page)} internal URLs")
    
    # Save the URL mapping to JSON file
    mapping_file = 'url_to_page_mapping.json'
    with open(mapping_file, 'w', encoding='utf-8') as f:
        json.dump(url_to_page, f, indent=2, ensure_ascii=False)
    print(f"URL mapping saved to: {mapping_file}")
    
    # Debug: Show first few mappings
    print("Sample URL mappings:")
    for i, (url, page) in enumerate(list(url_to_page.items())[:5]):
        print(f"  {url} → page {page}")
    if len(url_to_page) > 5:
        print("  ...")
    
    print("Step 2: Merging PDFs with bookmarks...")
    merger = PdfMerger()
    for top in manual['catalog']:
        top_label = top.get('label', None)
        if top_label:
            print(f"Processing top-level: {top_label}")
            add_pdfs_and_bookmarks(merger, top, [], parent=None, page_offset=len(merger.pages))
    
    # Output merged PDF to current directory
    output_path = 'merged_manual.pdf'
    merger.write(output_path)
    merger.close()
    print(f"Merged PDF created with bookmarks: {output_path}")
    print(f"Total pages in merged PDF: {len(merger.pages) if hasattr(merger, 'pages') else 'Unknown'}")
    
    print("\nFiles created:")
    print(f"  📄 {output_path} - Merged PDF with bookmarks")
    print(f"  📋 {mapping_file} - URL to page mapping for link processing")

if __name__ == '__main__':
    main()
