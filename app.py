from flask import Flask, request, jsonify, send_from_directory
from flask_cors import CORS
import os
import json
import zipfile
import shutil
from werkzeug.utils import secure_filename

app = Flask(__name__, static_folder='frontend/build')
CORS(app)

# Directory to store uploaded code files
UPLOAD_FOLDER = 'uploads'
# Directory to store label data
LABELS_FOLDER = 'labels'
# Directory to store project folders
PROJECTS_FOLDER = 'projects'

# Create directories if they don't exist
os.makedirs(UPLOAD_FOLDER, exist_ok=True)
os.makedirs(LABELS_FOLDER, exist_ok=True)
os.makedirs(PROJECTS_FOLDER, exist_ok=True)

@app.route('/api/upload', methods=['POST'])
def upload_file():
    """Upload a source code file for labeling"""
    if 'file' not in request.files:
        return jsonify({'error': 'No file part'}), 400
    
    file = request.files['file']
    if file.filename == '':
        return jsonify({'error': 'No selected file'}), 400
    
    # Save the file
    filename = os.path.join(UPLOAD_FOLDER, secure_filename(file.filename))
    file.save(filename)
    
    # Read the file content
    try:
        with open(filename, 'r') as f:
            content = f.read()
    except UnicodeDecodeError:
        # Handle binary files
        content = "Binary file - preview not available"
    
    # Create an empty label file
    label_filename = os.path.join(LABELS_FOLDER, f"{os.path.basename(filename)}.json")
    with open(label_filename, 'w') as f:
        json.dump({
            'filename': os.path.basename(filename),
            'labels': []
        }, f)
    
    return jsonify({
        'filename': os.path.basename(filename),
        'content': content,
        'message': 'File uploaded successfully'
    })

@app.route('/api/upload-folder', methods=['POST'])
def upload_folder():
    """Upload a zip file containing a folder of source code files"""
    if 'zipfile' not in request.files:
        return jsonify({'error': 'No zip file part'}), 400
    
    zipfile_obj = request.files['zipfile']
    if zipfile_obj.filename == '':
        return jsonify({'error': 'No selected zip file'}), 400
    
    project_name = request.form.get('projectName', 'unnamed_project')
    project_name = secure_filename(project_name)
    
    # Create a project directory
    project_dir = os.path.join(PROJECTS_FOLDER, project_name)
    if os.path.exists(project_dir):
        # If project already exists, create a new version
        i = 1
        while os.path.exists(f"{project_dir}_{i}"):
            i += 1
        project_dir = f"{project_dir}_{i}"
    
    os.makedirs(project_dir, exist_ok=True)
    
    # Save the zip file temporarily
    temp_zip_path = os.path.join(project_dir, 'temp.zip')
    zipfile_obj.save(temp_zip_path)
    
    # Extract the zip file
    with zipfile.ZipFile(temp_zip_path, 'r') as zip_ref:
        zip_ref.extractall(project_dir)
    
    # Remove the temporary zip file
    os.remove(temp_zip_path)
    
    # Get list of extracted files
    file_list = []
    for root, _, files in os.walk(project_dir):
        for file in files:
            rel_path = os.path.relpath(os.path.join(root, file), project_dir)
            file_list.append(rel_path)
    
    # Create a project info file
    project_info = {
        'name': project_name,
        'files': file_list,
        'created_at': os.path.getctime(project_dir)
    }
    
    with open(os.path.join(project_dir, 'project_info.json'), 'w') as f:
        json.dump(project_info, f)
    
    return jsonify({
        'project': project_name,
        'files': file_list,
        'message': f'Project {project_name} created with {len(file_list)} files'
    })

@app.route('/api/files', methods=['GET'])
def list_files():
    """List all uploaded files"""
    files = []
    for filename in os.listdir(UPLOAD_FOLDER):
        if os.path.isfile(os.path.join(UPLOAD_FOLDER, filename)):
            files.append(filename)
    return jsonify({'files': files})

@app.route('/api/projects', methods=['GET'])
def list_projects():
    """List all projects"""
    projects = []
    for project_name in os.listdir(PROJECTS_FOLDER):
        project_dir = os.path.join(PROJECTS_FOLDER, project_name)
        if os.path.isdir(project_dir):
            # Try to read project info
            info_path = os.path.join(project_dir, 'project_info.json')
            if os.path.exists(info_path):
                try:
                    with open(info_path, 'r') as f:
                        project_info = json.load(f)
                except:
                    project_info = {
                        'name': project_name,
                        'files': [],
                        'created_at': os.path.getctime(project_dir)
                    }
            else:
                # Count files in project
                file_count = 0
                for _, _, files in os.walk(project_dir):
                    file_count += len(files)
                
                project_info = {
                    'name': project_name,
                    'file_count': file_count,
                    'created_at': os.path.getctime(project_dir)
                }
            
            projects.append(project_info)
    
    return jsonify({'projects': projects})

@app.route('/api/projects/<project_name>', methods=['GET'])
def get_project_files(project_name):
    """Get files for a specific project"""
    project_dir = os.path.join(PROJECTS_FOLDER, project_name)
    if not os.path.exists(project_dir) or not os.path.isdir(project_dir):
        return jsonify({'error': 'Project not found'}), 404
    
    # Try to read project info
    info_path = os.path.join(project_dir, 'project_info.json')
    if os.path.exists(info_path):
        try:
            with open(info_path, 'r') as f:
                project_info = json.load(f)
                return jsonify(project_info)
        except:
            pass
    
    # If no info file or error reading it, generate file list
    file_list = []
    for root, _, files in os.walk(project_dir):
        for file in files:
            if file != 'project_info.json':
                rel_path = os.path.relpath(os.path.join(root, file), project_dir)
                file_list.append(rel_path)
    
    project_info = {
        'name': project_name,
        'files': file_list,
        'created_at': os.path.getctime(project_dir)
    }
    
    # Save the project info for future use
    with open(info_path, 'w') as f:
        json.dump(project_info, f)
    
    return jsonify(project_info)

@app.route('/api/file/<filename>', methods=['GET'])
def get_file(filename):
    """Get the content of a specific file"""
    filepath = os.path.join(UPLOAD_FOLDER, filename)
    if not os.path.exists(filepath):
        return jsonify({'error': 'File not found'}), 404
    
    try:
        with open(filepath, 'r') as f:
            content = f.read()
    except UnicodeDecodeError:
        # Handle binary files
        content = "Binary file - preview not available"
    
    return jsonify({
        'filename': filename,
        'content': content
    })

@app.route('/api/project-file/<project_name>/<path:file_path>', methods=['GET'])
def get_project_file(project_name, file_path):
    """Get the content of a specific file in a project"""
    # Ensure the file path is safe
    safe_path = os.path.normpath(file_path)
    if safe_path.startswith('..'):
        return jsonify({'error': 'Invalid file path'}), 400
    
    filepath = os.path.join(PROJECTS_FOLDER, project_name, safe_path)
    if not os.path.exists(filepath) or not os.path.isfile(filepath):
        return jsonify({'error': 'File not found'}), 404
    
    try:
        with open(filepath, 'r') as f:
            content = f.read()
    except UnicodeDecodeError:
        # Handle binary files
        content = "Binary file - preview not available"
    
    return jsonify({
        'project': project_name,
        'filename': safe_path,
        'content': content
    })

@app.route('/api/labels/<filename>', methods=['GET'])
def get_labels(filename):
    """Get labels for a specific file"""
    label_filename = os.path.join(LABELS_FOLDER, f"{filename}.json")
    if not os.path.exists(label_filename):
        return jsonify({
            'filename': filename,
            'labels': []
        })
    
    with open(label_filename, 'r') as f:
        labels = json.load(f)
    
    return jsonify(labels)

@app.route('/api/labels/<filename>', methods=['POST'])
def save_labels(filename):
    """Save labels for a specific file"""
    data = request.json
    
    label_filename = os.path.join(LABELS_FOLDER, f"{filename}.json")
    with open(label_filename, 'w') as f:
        json.dump(data, f)
    
    return jsonify({'message': 'Labels saved successfully'})

@app.route('/api/project-labels/<project_name>/<path:file_path>', methods=['GET'])
def get_project_labels(project_name, file_path):
    """Get labels for a specific file in a project"""
    # Ensure the file path is safe
    safe_path = os.path.normpath(file_path)
    if safe_path.startswith('..'):
        return jsonify({'error': 'Invalid file path'}), 400
    
    # Create a unique identifier for the file
    file_id = f"{project_name}_{safe_path.replace('/', '_')}"
    label_filename = os.path.join(LABELS_FOLDER, f"{file_id}.json")
    
    if not os.path.exists(label_filename):
        return jsonify({
            'project': project_name,
            'filename': safe_path,
            'labels': []
        })
    
    with open(label_filename, 'r') as f:
        labels = json.load(f)
    
    return jsonify(labels)

@app.route('/api/project-labels/<project_name>/<path:file_path>', methods=['POST'])
def save_project_labels(project_name, file_path):
    """Save labels for a specific file in a project"""
    # Ensure the file path is safe
    safe_path = os.path.normpath(file_path)
    if safe_path.startswith('..'):
        return jsonify({'error': 'Invalid file path'}), 400
    
    data = request.json
    
    # Create a unique identifier for the file
    file_id = f"{project_name}_{safe_path.replace('/', '_')}"
    label_filename = os.path.join(LABELS_FOLDER, f"{file_id}.json")
    
    with open(label_filename, 'w') as f:
        json.dump(data, f)
    
    return jsonify({'message': 'Labels saved successfully'})

@app.route('/api/analyze/<filename>', methods=['GET'])
def analyze_file(filename):
    """Perform basic analysis on the file to suggest labels"""
    filepath = os.path.join(UPLOAD_FOLDER, filename)
    if not os.path.exists(filepath):
        return jsonify({'error': 'File not found'}), 404
    
    # Simple rule-based analysis for demonstration
    with open(filepath, 'r') as f:
        content = f.readlines()
    
    suggestions = []
    for i, line in enumerate(content):
        line = line.strip().lower()
        
        # Simple rule-based suggestions
        if 'select' in line or 'from' in line or 'where' in line:
            suggestions.append({
                'line': i + 1,
                'text': content[i].strip(),
                'suggestion': 'data I/O',
                'confidence': 0.8
            })
        elif 'if' in line or 'else' in line or 'for' in line or 'while' in line:
            suggestions.append({
                'line': i + 1,
                'text': content[i].strip(),
                'suggestion': 'control structure',
                'confidence': 0.7
            })
        elif 'function' in line or 'procedure' in line or 'sub' in line:
            suggestions.append({
                'line': i + 1,
                'text': content[i].strip(),
                'suggestion': 'business logic',
                'confidence': 0.6
            })
    
    return jsonify({'suggestions': suggestions})

# API status endpoint
@app.route('/')
def index():
    return jsonify({
        'status': 'online',
        'message': 'LabelMate API is running',
        'endpoints': [
            # Individual file endpoints
            '/api/upload - Upload a file',
            '/api/files - List all files',
            '/api/file/<filename> - Get file content',
            '/api/labels/<filename> - Get or save labels',
            '/api/analyze/<filename> - Get analysis suggestions',
            
            # Project management endpoints
            '/api/upload-folder - Upload a project folder (zip file)',
            '/api/projects - List all projects',
            '/api/projects/<project_name> - Get files for a specific project',
            '/api/project-file/<project_name>/<file_path> - Get content of a file in a project',
            '/api/project-labels/<project_name>/<file_path> - Get or save labels for a file in a project'
        ]
    })

# Serve the React frontend
@app.route('/app', defaults={'path': ''})
@app.route('/app/<path:path>')
def serve(path):
    if path != "" and os.path.exists(app.static_folder + '/' + path):
        return send_from_directory(app.static_folder, path)
    else:
        return send_from_directory(app.static_folder, 'index.html')

if __name__ == '__main__':
    app.run(debug=True)
