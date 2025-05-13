import React, { useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import axios from 'axios';

const CodeEditor = () => {
  const { filename } = useParams();
  const [content, setContent] = useState('');
  const [labels, setLabels] = useState([]);
  const [suggestions, setSuggestions] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [selectedLabel, setSelectedLabel] = useState('');
  const [selectedLines, setSelectedLines] = useState([]);
  const [message, setMessage] = useState('');

  // Label categories based on the README specifications
  const labelCategories = [
    'business logic',
    'data I/O',
    'control structure',
    'business workflow',
    'user interface',
    'error handling',
    'security',
    'integration point'
  ];

  useEffect(() => {
    const fetchData = async () => {
      try {
        // Fetch file content
        const contentRes = await axios.get(`/api/file/${filename}`);
        setContent(contentRes.data.content);
        
        // Fetch existing labels
        const labelsRes = await axios.get(`/api/labels/${filename}`);
        setLabels(labelsRes.data.labels || []);
        
        // Get AI suggestions
        const suggestionsRes = await axios.get(`/api/analyze/${filename}`);
        setSuggestions(suggestionsRes.data.suggestions || []);
        
        setLoading(false);
      } catch (err) {
        setError('Error loading file data');
        setLoading(false);
      }
    };

    fetchData();
  }, [filename]);

  const handleLineClick = (lineNumber) => {
    // Toggle line selection
    if (selectedLines.includes(lineNumber)) {
      setSelectedLines(selectedLines.filter(line => line !== lineNumber));
    } else {
      setSelectedLines([...selectedLines, lineNumber]);
    }
  };

  const handleAddLabel = () => {
    if (!selectedLabel || selectedLines.length === 0) {
      setError('Please select a label category and at least one line');
      return;
    }

    // Create new label
    const newLabel = {
      id: Date.now().toString(),
      category: selectedLabel,
      lines: [...selectedLines].sort((a, b) => a - b),
      timestamp: new Date().toISOString()
    };

    // Add to labels array
    const updatedLabels = [...labels, newLabel];
    setLabels(updatedLabels);
    
    // Clear selection
    setSelectedLines([]);
    setSelectedLabel('');
    setError('');
    setMessage('Label added successfully');
    
    // Save to backend
    saveLabels(updatedLabels);
  };

  const handleRemoveLabel = (labelId) => {
    const updatedLabels = labels.filter(label => label.id !== labelId);
    setLabels(updatedLabels);
    saveLabels(updatedLabels);
    setMessage('Label removed');
  };

  const saveLabels = async (labelsData) => {
    try {
      await axios.post(`/api/labels/${filename}`, {
        filename,
        labels: labelsData
      });
    } catch (err) {
      setError('Error saving labels');
    }
  };

  const applySuggestion = (suggestion) => {
    setSelectedLines([suggestion.line]);
    setSelectedLabel(suggestion.suggestion);
  };

  if (loading) {
    return <div>Loading...</div>;
  }

  const contentLines = content.split('\n');

  return (
    <div className="editor-container">
      <h2>Code Editor: {filename}</h2>
      
      {error && <div className="alert alert-error">{error}</div>}
      {message && <div className="alert alert-success">{message}</div>}
      
      <div className="editor-layout">
        <div className="code-panel">
          <h3>Source Code</h3>
          <div className="code-container">
            <pre className="code-display">
              {contentLines.map((line, index) => (
                <div 
                  key={index} 
                  className={`code-line ${selectedLines.includes(index + 1) ? 'selected' : ''} ${
                    labels.some(label => label.lines.includes(index + 1)) ? 'labeled' : ''
                  }`}
                  onClick={() => handleLineClick(index + 1)}
                >
                  <span className="line-number">{index + 1}</span>
                  <span className="line-content">{line}</span>
                </div>
              ))}
            </pre>
          </div>
        </div>
        
        <div className="sidebar">
          <div className="labeling-panel">
            <h3>Add Label</h3>
            <p>Selected Lines: {selectedLines.length > 0 ? selectedLines.join(', ') : 'None'}</p>
            
            <div className="form-control">
              <label htmlFor="label-category">Label Category</label>
              <select 
                id="label-category" 
                value={selectedLabel}
                onChange={(e) => setSelectedLabel(e.target.value)}
              >
                <option value="">Select a category</option>
                {labelCategories.map(category => (
                  <option key={category} value={category}>
                    {category}
                  </option>
                ))}
              </select>
            </div>
            
            <button 
              className="btn btn-block" 
              onClick={handleAddLabel}
              disabled={!selectedLabel || selectedLines.length === 0}
            >
              Add Label
            </button>
          </div>
          
          <div className="suggestions-panel">
            <h3>AI Suggestions</h3>
            {suggestions.length === 0 ? (
              <p>No suggestions available</p>
            ) : (
              <div>
                {suggestions.map((suggestion, index) => (
                  <div key={index} className="suggestion-item card">
                    <p>Line {suggestion.line}: <strong>{suggestion.suggestion}</strong></p>
                    <p className="suggestion-text">{suggestion.text}</p>
                    <p>Confidence: {(suggestion.confidence * 100).toFixed(0)}%</p>
                    <button 
                      className="btn" 
                      onClick={() => applySuggestion(suggestion)}
                    >
                      Apply
                    </button>
                  </div>
                ))}
              </div>
            )}
          </div>
          
          <div className="labels-panel">
            <h3>Applied Labels</h3>
            {labels.length === 0 ? (
              <p>No labels added yet</p>
            ) : (
              <div>
                {labels.map(label => (
                  <div key={label.id} className="label-item card">
                    <p>
                      <strong>{label.category}</strong>
                    </p>
                    <p>Lines: {label.lines.join(', ')}</p>
                    <button 
                      className="btn" 
                      onClick={() => handleRemoveLabel(label.id)}
                    >
                      Remove
                    </button>
                  </div>
                ))}
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default CodeEditor;
