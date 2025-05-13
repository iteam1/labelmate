# LabelMate

## Overview

LabelMate is a semi-automated solution for migrating mainframe legacy systems, combining domain expert collaboration with AI. The AI assists in the inspection, assessment, and transformation of legacy code. This tool enables mainframe experts to label meaningful components in legacy code, which can then be used to train AI models for code transformation.

## Features

- Enable structured inspection and annotation of mainframe legacy codebase
- Involve mainframe experts to label meaningful components
- Support scalability, traceability, and validation
- Upload and manage both individual files and entire project folders
- Interactive code editor with line selection for labeling
- AI-assisted labeling suggestions
- Project management with folder structure visualization

## System Architecture

### Labeling Application

- UI for loading and navigating source code
- Allows tagging components (e.g., business logic, data I/O, control structures, business workflow, etc.)
- Stores labels in a structured format (JSON)

### Expert-AI Collaboration Layer

- Human-in-the-loop system for labeling guidance and refinement
- Semi-automated pre-labeling suggestions using static analysis or rule-based methods

### AI Migration Engine

- Uses labeled datasets to learn code transformation patterns
- Allows many migration strategies such as (autonomous, reengineering, hybrid, strangler pattern, etc.)
- Supports template-based or model-based code generation

### Validation & Traceability Module
- Run tests and checks on transformed code
- Tracks mappings from source to target code lines for auditability

## Setup and Installation

### Prerequisites

- Python 3.10 or higher
- Node.js 18.x or higher
- npm 9.x or higher

### Backend Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/labelmate.git
   cd labelmate
   ```

2. Create and activate a virtual environment:
   ```bash
   conda create -n labelmate python=3.10
   conda activate labelmate
   ```

3. Install backend dependencies:
   ```bash
   pip install -r requirements.txt
   ```

### Frontend Setup

1. Navigate to the frontend directory:
   ```bash
   cd frontend
   ```

2. Install frontend dependencies:
   ```bash
   npm install
   ```

## Running the Application

### Start the Backend Server

```bash
python app.py
```

The Flask backend will run on http://127.0.0.1:5000/

### Start the Frontend Development Server

```bash
cd frontend
npm start
```

The React frontend will run on http://localhost:3000/

## API Endpoints

### Individual File Management

- `POST /api/upload` - Upload a single file
- `GET /api/files` - List all uploaded files
- `GET /api/file/<filename>` - Get content of a specific file
- `GET /api/labels/<filename>` - Get labels for a specific file
- `POST /api/labels/<filename>` - Save labels for a specific file
- `GET /api/analyze/<filename>` - Get analysis suggestions for a file

### Project Management

- `POST /api/upload-folder` - Upload a project folder (zip file)
- `GET /api/projects` - List all projects
- `GET /api/projects/<project_name>` - Get files for a specific project
- `GET /api/project-file/<project_name>/<file_path>` - Get content of a file in a project
- `GET /api/project-labels/<project_name>/<file_path>` - Get labels for a file in a project
- `POST /api/project-labels/<project_name>/<file_path>` - Save labels for a file in a project

## Usage Guide

### Individual File Management

1. **Upload a File**:
   - Navigate to "Upload File" in the header
   - Select a file from your computer (supported formats include COBOL, JCL, Assembler, etc.)
   - Click "Upload File"

2. **View and Manage Files**:
   - Navigate to "Files" in the header
   - Click on a file to open it in the editor

3. **Label Code**:
   - In the code editor, click on lines to select them
   - Choose a label category from the dropdown (business logic, data I/O, etc.)
   - Click "Add Label" to apply the label
   - Labels are automatically saved to the server

### Project Management

1. **Upload a Project**:
   - Navigate to "Upload Project" in the header
   - Enter a project name
   - Select a zip file containing your source code
   - Click "Upload Project"

2. **Navigate Projects**:
   - The home page shows all your projects
   - Click on a project to view its file structure
   - Use the folder tree to navigate through directories
   - Search for specific files within a project

3. **Label Code in Projects**:
   - Click on any file in the project to open it in the editor
   - Select lines and apply labels just like with individual files
   - Labels are saved per file within the project context

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the terms of the license included in the repository.
