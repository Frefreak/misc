import os
import json
from PyPDF2 import PdfMerger, PdfReader

FOLDER = './data/'  # Your root folder
MANUAL_JSON = os.path.join(os.path.dirname(__file__), 'manual.json')

# Helper to build the PDF path from label hierarchy
def get_pdf_path(label_path):
    safe_labels = [label.replace('/', '_') for label in label_path]
    dir_path = os.path.join(FOLDER, *safe_labels[:-1])
    pdf_path = os.path.join(dir_path, f"{safe_labels[-1]}.pdf")
    return pdf_path

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
    merger = PdfMerger()
    for top in manual['catalog']:
        top_label = top.get('label', None)
        if top_label:
            print(f"Processing top-level: {top_label}")
            add_pdfs_and_bookmarks(merger, top, [], parent=None, page_offset=len(merger.pages))
    output_path = os.path.join(FOLDER, 'merged_manual.pdf')
    merger.write(output_path)
    merger.close()
    print("Merged PDF created with bookmarks!")

if __name__ == '__main__':
    main()
