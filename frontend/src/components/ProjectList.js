import React, { useState, useEffect } from 'react';
import { Link } from 'react-router-dom';
import axios from 'axios';

const ProjectList = () => {
  const [projects, setProjects] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');

  useEffect(() => {
    const fetchProjects = async () => {
      try {
        const res = await axios.get('/api/projects');
        setProjects(res.data.projects);
        setLoading(false);
      } catch (err) {
        setError('Error fetching projects');
        setLoading(false);
      }
    };

    fetchProjects();
  }, []);

  if (loading) {
    return <div>Loading projects...</div>;
  }

  const formatDate = (timestamp) => {
    return new Date(timestamp * 1000).toLocaleDateString();
  };

  return (
    <div className="project-list">
      <h2>Projects</h2>
      
      {error && <div className="alert alert-error">{error}</div>}
      
      <div className="actions">
        <Link to="/upload-folder" className="btn">
          Upload New Project
        </Link>
        <Link to="/upload" className="btn">
          Upload Single File
        </Link>
      </div>
      
      {projects.length === 0 ? (
        <div className="card">
          <p>No projects have been uploaded yet.</p>
          <Link to="/upload-folder" className="btn">
            Upload Your First Project
          </Link>
        </div>
      ) : (
        <div className="projects-grid">
          {projects.map((project) => (
            <div key={project.name} className="card project-card">
              <h3>{project.name}</h3>
              <p>
                <strong>Files:</strong> {project.files ? project.files.length : project.file_count || 0}
              </p>
              <p>
                <strong>Created:</strong> {formatDate(project.created_at)}
              </p>
              <Link to={`/projects/${project.name}`} className="btn">
                Open Project
              </Link>
            </div>
          ))}
        </div>
      )}
    </div>
  );
};

export default ProjectList;
