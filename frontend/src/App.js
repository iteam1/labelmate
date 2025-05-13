import React from 'react';
import { BrowserRouter as Router, Route, Switch } from 'react-router-dom';
import './App.css';
import './CodeEditor.css';
import './styles/ProjectStyles.css';
import Header from './components/Header';
import FileUpload from './components/FileUpload';
import FileList from './components/FileList';
import CodeEditor from './components/CodeEditor';
import FolderUpload from './components/FolderUpload';
import ProjectList from './components/ProjectList';
import ProjectFiles from './components/ProjectFiles';
import ProjectEditor from './components/ProjectEditor';

function App() {
  return (
    <Router>
      <div className="App">
        <Header />
        <div className="container">
          <Switch>
            <Route exact path="/" component={ProjectList} />
            <Route exact path="/files" component={FileList} />
            <Route path="/upload" component={FileUpload} />
            <Route path="/upload-folder" component={FolderUpload} />
            <Route path="/editor/:filename" component={CodeEditor} />
            <Route exact path="/projects" component={ProjectList} />
            <Route exact path="/projects/:projectName" component={ProjectFiles} />
            <Route path="/project-editor/:projectName/:filePath+" component={ProjectEditor} />
          </Switch>
        </div>
      </div>
    </Router>
  );
}

export default App;
