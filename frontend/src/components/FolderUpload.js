import React, { useState } from 'react';
import { useHistory } from 'react-router-dom';
import axios from 'axios';

const FolderUpload = () => {
  const [zipFile, setZipFile] = useState(null);
  const [projectName, setProjectName] = useState('');
  const [message, setMessage] = useState('');
  const [error, setError] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const history = useHistory();

  const onFileChange = (e) => {
    setZipFile(e.target.files[0]);
    setMessage('');
    setError('');
  };

  const onSubmit = async (e) => {
    e.preventDefault();
    
    if (!zipFile) {
      setError('Please select a zip file to upload');
      return;
    }

    if (!projectName.trim()) {
      setError('Please enter a project name');
      return;
    }

    const formData = new FormData();
    formData.append('zipfile', zipFile);
    formData.append('projectName', projectName);

    setIsLoading(true);

    try {
      const res = await axios.post('/api/upload-folder', formData, {
        headers: {
          'Content-Type': 'multipart/form-data'
        }
      });

      setMessage(`Project "${res.data.project}" created successfully with ${res.data.files.length} files`);
      setIsLoading(false);
      
      // Redirect to the project page after a short delay
      setTimeout(() => {
        history.push(`/projects/${res.data.project}`);
      }, 1500);
    } catch (err) {
      setError(err.response?.data?.error || 'Error uploading project');
      setIsLoading(false);
    }
  };

  return (
    <div className="upload-container">
      <h2>Upload Project Folder</h2>
      <p>Upload a zip file containing your mainframe legacy code project for labeling and analysis.</p>
      
      {message && <div className="alert alert-success">{message}</div>}
      {error && <div className="alert alert-error">{error}</div>}
      
      <form onSubmit={onSubmit} className="card">
        <div className="form-control">
          <label htmlFor="projectName">Project Name</label>
          <input 
            type="text" 
            id="projectName" 
            value={projectName}
            onChange={(e) => setProjectName(e.target.value)}
            placeholder="Enter a name for your project"
            required
          />
        </div>
        
        <div className="form-control">
          <label htmlFor="zipfile">Select Zip File</label>
          <input 
            type="file" 
            id="zipfile" 
            onChange={onFileChange} 
            accept=".zip"
          />
          <small>Upload a .zip file containing your source code files</small>
        </div>
        
        <button 
          type="submit" 
          className="btn btn-block" 
          disabled={isLoading}
        >
          {isLoading ? 'Uploading...' : 'Upload Project'}
        </button>
      </form>
      
      <div className="upload-info card">
        <h3>Project Upload Guidelines</h3>
        <ul style={{ textAlign: 'left' }}>
          <li>Create a zip file of your project folder</li>
          <li>Ensure the zip file contains all necessary source files</li>
          <li>Maintain the original folder structure in the zip file</li>
          <li>Maximum zip file size: 50MB</li>
          <li>Supported file types include COBOL, JCL, Assembler, PL/I, etc.</li>
        </ul>
      </div>
    </div>
  );
};

export default FolderUpload;
