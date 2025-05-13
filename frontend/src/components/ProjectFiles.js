import React, { useState, useEffect } from 'react';
import { Link, useParams } from 'react-router-dom';
import axios from 'axios';

const ProjectFiles = () => {
  const { projectName } = useParams();
  const [project, setProject] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [searchTerm, setSearchTerm] = useState('');
  const [expandedFolders, setExpandedFolders] = useState({});

  useEffect(() => {
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
  }, [projectName]);

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
              <div key={file.path} className="file-item">
                <span className="file-icon">📄</span>
                <Link to={`/project-editor/${projectName}/${file.path}`}>
                  {file.name}
                </Link>
              </div>
            ))}
          </div>
        )}
      </div>
    );
  };

  return (
    <div className="project-files">
      <h2>Project: {project.name}</h2>
      
      <div className="project-actions">
        <Link to="/projects" className="btn">
          Back to Projects
        </Link>
      </div>
      
      <div className="search-bar">
        <input
          type="text"
          placeholder="Search files..."
          value={searchTerm}
          onChange={(e) => setSearchTerm(e.target.value)}
        />
      </div>
      
      <div className="file-browser card">
        <h3>Files ({project.files.length})</h3>
        
        {searchTerm ? (
          // Show flat list of search results
          <div className="search-results">
            {filteredFiles.length === 0 ? (
              <p>No files match your search.</p>
            ) : (
              filteredFiles.map(filePath => (
                <div key={filePath} className="file-item">
                  <span className="file-icon">📄</span>
                  <Link to={`/project-editor/${projectName}/${filePath}`}>
                    {filePath}
                  </Link>
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
              <div key={file.path} className="file-item">
                <span className="file-icon">📄</span>
                <Link to={`/project-editor/${projectName}/${file.path}`}>
                  {file.name}
                </Link>
              </div>
            ))}
            
            {(!fileTree.folders || Object.keys(fileTree.folders).length === 0) && 
             (!fileTree.files || fileTree.files.length === 0) && (
              <p>No files found in this project.</p>
            )}
          </div>
        )}
      </div>
    </div>
  );
};

export default ProjectFiles;
