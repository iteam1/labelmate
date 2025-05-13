import React, { useState, useEffect } from 'react';
import { Link, useParams, useHistory, useLocation } from 'react-router-dom';
import axios from 'axios';

const ProjectFiles = () => {
  const { projectName } = useParams();
  const history = useHistory();
  const location = useLocation();
  const [project, setProject] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [searchTerm, setSearchTerm] = useState('');
  const [expandedFolders, setExpandedFolders] = useState({});
  const [selectedFile, setSelectedFile] = useState(null);
  const [fileContent, setFileContent] = useState('');
  const [activeTab, setActiveTab] = useState(null);
  const [successMessage, setSuccessMessage] = useState('');

  useEffect(() => {
    // Check if we have project data from the upload component
    if (location.state && location.state.fromUpload && location.state.projectData) {
      setProject(location.state.projectData);
      setSuccessMessage(`Project "${location.state.projectData.project}" uploaded successfully with ${location.state.projectData.files.length} files`);
      setLoading(false);
      
      // Clear the location state after using it
      history.replace({
        pathname: location.pathname,
        state: {}
      });
    } else {
      // Fetch project data if not coming from upload
      const fetchProjectFiles = async () => {
        try {
          const res = await axios.get(`/api/projects/${projectName}`);
          setProject(res.data);
          setLoading(false);
        } catch (err) {
          setError('Error fetching project files');
          setLoading(false);
        }
      };

      fetchProjectFiles();
    }
  }, [projectName, location, history]);

  if (loading) {
    return <div>Loading project files...</div>;
  }

  if (error) {
    return <div className="alert alert-error">{error}</div>;
  }

  if (!project) {
    return <div className="alert alert-error">Project not found</div>;
  }

  // Organize files into a folder structure
  const organizeFilesByFolder = () => {
    const fileTree = {};
    
    project.files.forEach(filePath => {
      const parts = filePath.split('/');
      let currentLevel = fileTree;
      
      // Process each part of the path
      for (let i = 0; i < parts.length; i++) {
        const part = parts[i];
        
        // If this is the last part (the file itself)
        if (i === parts.length - 1) {
          if (!currentLevel.files) {
            currentLevel.files = [];
          }
          currentLevel.files.push({
            name: part,
            path: filePath
          });
        } else {
          // This is a directory
          if (!currentLevel.folders) {
            currentLevel.folders = {};
          }
          if (!currentLevel.folders[part]) {
            currentLevel.folders[part] = {};
          }
          currentLevel = currentLevel.folders[part];
        }
      }
    });
    
    return fileTree;
  };

  const fileTree = organizeFilesByFolder();

  // Filter files based on search term
  const filteredFiles = searchTerm
    ? project.files.filter(file => 
        file.toLowerCase().includes(searchTerm.toLowerCase())
      )
    : null;

  // Toggle folder expansion
  const toggleFolder = (folderPath) => {
    setExpandedFolders(prev => ({
      ...prev,
      [folderPath]: !prev[folderPath]
    }));
  };
  
  // Handle file selection
  const handleFileSelect = async (filePath) => {
    try {
      setSelectedFile(filePath);
      setActiveTab(filePath);
      
      // Fetch file content
      const res = await axios.get(`/api/project-file/${projectName}/${filePath}`);
      setFileContent(res.data.content);
      
      // Update URL without navigating away from the page
      history.push(`/projects/${projectName}?file=${encodeURIComponent(filePath)}`);
    } catch (err) {
      setError(`Error loading file: ${filePath}`);
    }
  };
  
  // Open file in editor
  const openInEditor = (filePath) => {
    history.push(`/project-editor/${projectName}/${filePath}`);
  };

  // Render a folder and its contents recursively
  const renderFolder = (folderContent, folderName, folderPath = '') => {
    const currentPath = folderPath ? `${folderPath}/${folderName}` : folderName;
    const isExpanded = expandedFolders[currentPath] !== false; // Default to expanded
    
    return (
      <div key={currentPath} className="folder">
        <div 
          className="folder-name" 
          onClick={() => toggleFolder(currentPath)}
        >
          <span className="folder-icon">
            {isExpanded ? '📂' : '📁'}
          </span>
          <span>{folderName}</span>
        </div>
        
        {isExpanded && (
          <div className="folder-contents">
            {folderContent.folders && Object.keys(folderContent.folders).map(subFolderName => 
              renderFolder(folderContent.folders[subFolderName], subFolderName, currentPath)
            )}
            
            {folderContent.files && folderContent.files.map(file => (
              <div 
                key={file.path} 
                className={`file-item ${selectedFile === file.path ? 'selected' : ''}`}
                onClick={() => handleFileSelect(file.path)}
              >
                <span className="file-icon">📄</span>
                <span className="file-name">{file.name}</span>
              </div>
            ))}
          </div>
        )}
      </div>
    );
  };
  
  // Get file extension
  const getFileExtension = (filename) => {
    return filename ? filename.split('.').pop().toLowerCase() : '';
  };
  
  // Get language from extension
  const getLanguageFromExtension = (ext) => {
    const extensionMap = {
      'cob': 'COBOL',
      'cbl': 'COBOL',
      'jcl': 'JCL',
      'asm': 'Assembler',
      'pli': 'PL/I',
      'nat': 'Natural',
      'rpg': 'RPG',
      'c': 'C',
      'cpp': 'C++',
      'h': 'C/C++ Header',
      'java': 'Java',
      'js': 'JavaScript',
      'py': 'Python',
      'sql': 'SQL',
      'txt': 'Text',
      'cpy': 'COBOL Copybook'
    };
    
    return extensionMap[ext] || 'Unknown';
  };

  return (
    <div className="vscode-layout">
      {/* Project Explorer Sidebar */}
      <div className="sidebar-panel">
        {successMessage && (
          <div className="success-message">
            {successMessage}
            <button className="close-message" onClick={() => setSuccessMessage('')}>×</button>
          </div>
        )}
        <div className="sidebar-header">
          <h3>PROJECT EXPLORER</h3>
          <div className="project-name">{project.name}</div>
        </div>
        
        <div className="search-bar">
          <input
            type="text"
            placeholder="Search files..."
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
          />
        </div>
        
        <div className="file-browser">
          {searchTerm ? (
            // Show flat list of search results
            <div className="search-results">
              {filteredFiles.length === 0 ? (
                <p>No files match your search.</p>
              ) : (
                filteredFiles.map(filePath => (
                  <div 
                    key={filePath} 
                    className={`file-item ${selectedFile === filePath ? 'selected' : ''}`}
                    onClick={() => handleFileSelect(filePath)}
                  >
                    <span className="file-icon">📄</span>
                    <span className="file-name">{filePath.split('/').pop()}</span>
                  </div>
                ))
              )}
            </div>
          ) : (
            // Show folder tree structure
            <div className="folder-tree">
              {fileTree.folders && Object.keys(fileTree.folders).map(folderName => 
                renderFolder(fileTree.folders[folderName], folderName)
              )}
              
              {fileTree.files && fileTree.files.map(file => (
                <div 
                  key={file.path} 
                  className={`file-item ${selectedFile === file.path ? 'selected' : ''}`}
                  onClick={() => handleFileSelect(file.path)}
                >
                  <span className="file-icon">📄</span>
                  <span className="file-name">{file.name}</span>
                </div>
              ))}
              
              {(!fileTree.folders || Object.keys(fileTree.folders).length === 0) && 
               (!fileTree.files || fileTree.files.length === 0) && (
                <p>No files found in this project.</p>
              )}
            </div>
          )}
        </div>
        
        <div className="sidebar-footer">
          <Link to="/projects" className="btn btn-sm">
            Back to Projects
          </Link>
        </div>
      </div>
      
      {/* Main Content Area */}
      <div className="main-content">
        {/* Tabs Bar */}
        <div className="tabs-container">
          {activeTab && (
            <div className="tab active">
              <span>{activeTab.split('/').pop()}</span>
              <button className="close-tab" onClick={() => {
                setActiveTab(null);
                setSelectedFile(null);
                setFileContent('');
              }}>×</button>
            </div>
          )}
        </div>
        
        {/* File Content Area */}
        {selectedFile ? (
          <div className="file-content-area">
            <div className="file-header">
              <div className="file-info">
                <span className="file-path">{selectedFile}</span>
                <span className="file-language">{getLanguageFromExtension(getFileExtension(selectedFile))}</span>
              </div>
              <div className="file-actions">
                <button 
                  className="btn btn-primary" 
                  onClick={() => openInEditor(selectedFile)}
                >
                  Open in Editor
                </button>
              </div>
            </div>
            
            <div className="code-preview">
              <pre className="code-display">
                {fileContent.split('\n').map((line, index) => (
                  <div key={index} className="code-line">
                    <span className="line-number">{index + 1}</span>
                    <span className="line-content">{line}</span>
                  </div>
                ))}
              </pre>
            </div>
          </div>
        ) : (
          <div className="welcome-screen">
            <h2>Welcome to {project.name}</h2>
            <p>Select a file from the project explorer to view its contents.</p>
            <p>This project contains {project.files.length} files.</p>
          </div>
        )}
      </div>
    </div>
  );
};

export default ProjectFiles;
