import React from 'react';

const Header = () => {
  const handleNavigation = (path) => {
    window.location.href = path;
  };
  
  return (
    <header className="header" style={headerStyle}>
      <div className="logo">
        <h1 onClick={() => handleNavigation('/')} style={{cursor: 'pointer'}}>LabelMate</h1>
      </div>
      <nav>
        <ul>
          <li>
            <a href="/" style={linkStyle}>Projects</a>
          </li>
          <li>
            <a href="/files" style={linkStyle}>Files</a>
          </li>
          <li>
            <a href="/upload" style={linkStyle}>Upload File</a>
          </li>
          <li>
            <a href="/upload-folder" style={linkStyle}>Upload Project</a>
          </li>
        </ul>
      </nav>
    </header>
  );
};

const headerStyle = {
  background: '#333',
  color: '#fff',
  textAlign: 'center',
  padding: '10px',
  display: 'flex',
  justifyContent: 'space-between',
  alignItems: 'center',
};

const linkStyle = {
  color: '#fff',
  textDecoration: 'none',
  margin: '0 15px',
};

export default Header;
