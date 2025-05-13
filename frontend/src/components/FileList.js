import React, { useState, useEffect } from 'react';
import { Link } from 'react-router-dom';
import axios from 'axios';

const FileList = () => {
  const [files, setFiles] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');

  useEffect(() => {
    const fetchFiles = async () => {
      try {
        const res = await axios.get('/api/files');
        setFiles(res.data.files);
        setLoading(false);
      } catch (err) {
        setError('Error fetching files');
        setLoading(false);
      }
    };

    fetchFiles();
  }, []);

  if (loading) {
    return <div>Loading...</div>;
  }

  return (
    <div className="file-list">
      <h2>Available Files</h2>
      
      {error && <div className="alert alert-error">{error}</div>}
      
      {files.length === 0 ? (
        <div className="card">
          <p>No files have been uploaded yet.</p>
          <Link to="/upload" className="btn">
            Upload a File
          </Link>
        </div>
      ) : (
        <div>
          <p>Select a file to open in the code editor for labeling:</p>
          
          <div className="files-grid">
            {files.map((filename) => (
              <div key={filename} className="card file-card">
                <h3>{filename}</h3>
                <Link to={`/editor/${filename}`} className="btn">
                  Open Editor
                </Link>
              </div>
            ))}
          </div>
          
          <div className="upload-more" style={{ marginTop: '20px' }}>
            <Link to="/upload" className="btn">
              Upload Another File
            </Link>
          </div>
        </div>
      )}
    </div>
  );
};

export default FileList;
