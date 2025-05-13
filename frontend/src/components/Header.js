import React from 'react';
import { Link } from 'react-router-dom';

const Header = () => {
  return (
    <header className="header" style={headerStyle}>
      <div className="logo">
        <h1>LabelMate</h1>
      </div>
      <nav>
        <ul>
          <li>
            <Link to="/" style={linkStyle}>Projects</Link>
          </li>
          <li>
            <Link to="/files" style={linkStyle}>Files</Link>
          </li>
          <li>
            <Link to="/upload" style={linkStyle}>Upload File</Link>
          </li>
          <li>
            <Link to="/upload-folder" style={linkStyle}>Upload Project</Link>
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
