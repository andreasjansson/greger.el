# 🌟 Emacs Lisp Quiz Challenge

A comprehensive, gamified quiz application to test your knowledge of Emacs Lisp, featuring 100 carefully crafted questions based on the official GNU Emacs Lisp Reference Manual.

## ✨ Features

### 📚 Comprehensive Question Database
- **100 multiple-choice questions** covering all aspects of Emacs Lisp
- Questions sourced from **official GNU Emacs documentation**
- **Progressive difficulty**: Beginner (20) → Intermediate (40) → Advanced (40)
- Topics include:
  - Basic syntax and data types
  - Functions and macros
  - Control structures and iteration
  - Buffer manipulation and text processing
  - Advanced topics like closures, advice, and bytecode compilation

### 🎮 Gamification Elements
- **Dynamic scoring system** with difficulty-based points
- **Streak bonuses** for consecutive correct answers
- **Real-time progress tracking** with visual indicators
- **Performance categories** from "Keep Learning" to "Emacs Lisp Master"
- **Star-based difficulty indicators** (★ to ★★★)

### 💾 Smart State Management
- **Auto-save progress** every 5 seconds
- **Resume capability** - continue where you left off
- **Local storage persistence** across browser sessions
- **State validation** to prevent corruption

### 🔗 Social Features
- **Shareable results** with base64-encoded URLs
- **Performance comparison** via shared links
- **Copy-to-clipboard** functionality for easy sharing

### 🛠 Developer Features
- **Test mode** (`?test=true`) with only 3 questions for development
- **Keyboard shortcuts** for efficient navigation:
  - `1-4`: Select options
  - `Enter`: Submit answer or next question
  - `Escape`: Close feedback modal

### 🎨 User Experience
- **Dark, colorful theme** with smooth animations
- **Responsive design** for desktop and mobile
- **Accessibility features** with clear visual feedback
- **Loading animations** and micro-interactions
- **Reference links** to official documentation

## 🚀 Quick Start

1. **Open `index.html`** in a modern web browser
2. **Click "Begin Quest"** to start the full 100-question challenge
3. **Or use test mode**: Add `?test=true` to the URL for a 3-question demo

## 📖 Question Categories

### Beginner (Questions 1-20) ⭐
- Basic functions: `car`, `cdr`, `cons`, `list`
- Variable assignment with `setq` and `let`
- Simple predicates and conditionals
- Comments and basic syntax
- Interactive functions

### Intermediate (Questions 21-60) ⭐⭐
- Higher-order functions: `mapcar`, `apply`, `funcall`
- Lambda expressions and closures
- Error handling with `condition-case`
- Association lists and property lists
- Hooks and customization

### Advanced (Questions 61-100) ⭐⭐⭐
- Macro system and `defmacro`
- Lexical vs dynamic scoping
- Advice system and function modification
- Buffer-local variables and narrowing
- Garbage collection and memory management
- Hash tables and advanced data structures

## 🎯 Scoring System

- **Beginner questions**: 10 points base
- **Intermediate questions**: 20 points base  
- **Advanced questions**: 30 points base
- **Streak bonuses**:
  - 3+ correct: +20% points
  - 5+ correct: +50% points

## 📱 Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `1-4` | Select answer option |
| `Enter` | Submit answer / Next question |
| `Escape` | Close feedback modal |

## 🔧 Technical Implementation

### Architecture
- **Vanilla JavaScript** - No external dependencies
- **CSS Grid and Flexbox** for responsive layouts
- **Local Storage API** for state persistence
- **Base64 encoding** for shareable URLs

### Files Structure
```
emacs-quiz/
├── index.html          # Main application shell
├── style.css           # Dark theme and animations
├── questions.js        # Question database (100 questions)
├── quiz.js            # Main application logic
└── README.md          # Documentation
```

### Browser Compatibility
- **Modern browsers** with ES6+ support
- **Local Storage** required for state saving
- **Responsive design** for mobile and desktop

## 📚 References

All questions are based on official Emacs documentation:

- [GNU Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/elisp.html)
- [An Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/eintr.html)

Each question includes:
- **Detailed explanation** of the correct answer
- **Reference link** to the relevant manual section
- **Context** from official documentation

## 🤝 Contributing

To add more questions or improve the quiz:

1. **Questions**: Add to `questions.js` following the existing format
2. **Features**: Modify `quiz.js` for new functionality  
3. **Styling**: Update `style.css` for visual changes

### Question Format
```javascript
{
    id: 101,
    difficulty: 'intermediate', // beginner, intermediate, advanced
    question: 'What does this function do?',
    options: [
        'Option A',
        'Option B', 
        'Option C',
        'Option D'
    ],
    correct: 1, // 0-indexed
    explanation: 'Detailed explanation...',
    reference: 'Manual section name',
    referenceUrl: 'https://...'
}
```

## 📄 License

This quiz is educational and references content from the GNU Emacs Lisp Reference Manual, which is distributed under the GNU Free Documentation License.

## 🎉 Acknowledgments

- **GNU Project** for the comprehensive Emacs Lisp documentation
- **Emacs community** for maintaining excellent resources
- **Richard Stallman** and contributors for creating Emacs

---

**Ready to test your Emacs Lisp mastery?** 🚀

[Start the Quiz](./index.html) | [Test Mode](./index.html?test=true)
