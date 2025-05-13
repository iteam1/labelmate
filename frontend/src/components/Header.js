import React from 'react';

const Header = () => {
  const handleNavigation = (path) => {
    window.location.href = path;
  };
  
  return (
    <header className="app-header">
      <div className="header-container">
        <div className="logo">
          <h1 onClick={() => handleNavigation('/')}>
            <span className="logo-label">Label</span>
            <span className="logo-mate">Mate</span>
            <span className="logo-tag">Legacy Code Migration Assistant</span>
          </h1>
        </div>
        <nav className="main-nav">
          <ul>
            <li>
              <a href="/" className="nav-link">
                <i className="nav-icon projects-icon">📂</i>
                <span>Projects</span>
              </a>
            </li>
            <li>
              <a href="/files" className="nav-link">
                <i className="nav-icon files-icon">📄</i>
                <span>Files</span>
              </a>
            </li>
            <li>
              <a href="/upload" className="nav-link">
                <i className="nav-icon upload-icon">⬆️</i>
                <span>Upload File</span>
              </a>
            </li>
            <li>
              <a href="/upload-folder" className="nav-link">
                <i className="nav-icon folder-icon">📁</i>
                <span>Upload Project</span>
              </a>
            </li>
          </ul>
        </nav>
      </div>
    </header>
  );
};

// Styles moved to CSS file

export default Header;
