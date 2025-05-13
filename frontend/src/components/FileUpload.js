import React, { useState } from 'react';
import { useHistory } from 'react-router-dom';
import axios from 'axios';

const FileUpload = () => {
  const [file, setFile] = useState(null);
  const [message, setMessage] = useState('');
  const [error, setError] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const history = useHistory();

  const onFileChange = (e) => {
    setFile(e.target.files[0]);
    setMessage('');
    setError('');
  };

  const onSubmit = async (e) => {
    e.preventDefault();
    
    if (!file) {
      setError('Please select a file to upload');
      return;
    }

    const formData = new FormData();
    formData.append('file', file);

    setIsLoading(true);

    try {
      const res = await axios.post('/api/upload', formData, {
        headers: {
          'Content-Type': 'multipart/form-data'
        }
      });

      setMessage('File uploaded successfully');
      setIsLoading(false);
      
      // Redirect to the editor page after a short delay
      setTimeout(() => {
        history.push(`/editor/${res.data.filename}`);
      }, 1500);
    } catch (err) {
      setError(err.response?.data?.error || 'Error uploading file');
      setIsLoading(false);
    }
  };

  return (
    <div className="upload-container">
      <h2>Upload Legacy Code File</h2>
      <p>Select a mainframe code file to upload for labeling and analysis.</p>
      
      {message && <div className="alert alert-success">{message}</div>}
      {error && <div className="alert alert-error">{error}</div>}
      
      <form onSubmit={onSubmit} className="card">
        <div className="form-control">
          <label htmlFor="file">Select File</label>
          <input 
            type="file" 
            id="file" 
            onChange={onFileChange} 
          />
        </div>
        
        <button 
          type="submit" 
          className="btn btn-block" 
          disabled={isLoading}
        >
          {isLoading ? 'Uploading...' : 'Upload File'}
        </button>
      </form>
      
      <div className="upload-info card">
        <h3>Supported File Types</h3>
        <ul style={{ textAlign: 'left' }}>
          <li>COBOL (.cob, .cbl)</li>
          <li>JCL (.jcl)</li>
          <li>Assembler (.asm)</li>
          <li>PL/I (.pli)</li>
          <li>Natural (.nat)</li>
          <li>RPG (.rpg)</li>
          <li>Text files (.txt)</li>
        </ul>
      </div>
    </div>
  );
};

export default FileUpload;
