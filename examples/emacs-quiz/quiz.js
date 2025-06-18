// Emacs Lisp Quiz - Main Application Logic
class EmacsLispQuiz {
    constructor() {
        this.questions = [];
        this.currentQuestionIndex = 0;
        this.score = 0;
        this.streak = 0;
        this.maxStreak = 0;
        this.startTime = null;
        this.selectedAnswer = null;
        this.isTestMode = false;
        this.isSharedResults = false;
        this.sharedTestMode = false;
        
        // State management
        this.state = {
            screen: 'start', // start, quiz, results
            answeredQuestions: [],
            totalTime: 0,
            questionAnswered: false
        };
        
        this.init();
    }
    
    init() {
        // Check for test mode
        const urlParams = new URLSearchParams(window.location.search);
        this.isTestMode = urlParams.get('test') === 'true';
        
        // Load questions based on mode
        this.questions = this.isTestMode ? [...TEST_QUESTIONS] : [...QUIZ_QUESTIONS];
        
        // Bind events
        this.bindEvents();
        
        // Check for shared results first
        const hasSharedResults = this.checkSharedResults();
        
        if (!hasSharedResults) {
            // Try to restore saved state only if not showing shared results
            this.loadState();
            
            // Show appropriate screen
            this.showScreen(this.state.screen);
        }
        
        if (this.state.screen === 'quiz') {
            // If current question was already answered, show feedback
            if (this.state.questionAnswered) {
                this.displayQuestionWithFeedback();
            } else {
                this.displayQuestion();
            }
            this.updateStats();
        }
    }
    
    bindEvents() {
        // Start button
        document.getElementById('start-btn').addEventListener('click', () => {
            this.startQuiz();
        });
        
        // Submit answer button
        document.getElementById('submit-answer').addEventListener('click', () => {
            this.submitAnswer();
        });
        
        // Next question button
        document.getElementById('next-question').addEventListener('click', () => {
            this.nextQuestion();
        });
        
        // Restart quiz button
        document.getElementById('restart-quiz').addEventListener('click', () => {
            this.restartQuiz();
        });
        
        // Share results button
        document.getElementById('share-results').addEventListener('click', () => {
            this.shareResults();
        });
        
        // Copy link button
        document.getElementById('copy-link').addEventListener('click', () => {
            this.copyShareLink();
        });
        
        // Handle option selection
        document.addEventListener('click', (e) => {
            if (e.target.classList.contains('option')) {
                this.selectOption(e.target);
            }
        });
        
        // Handle keyboard shortcuts
        document.addEventListener('keydown', (e) => {
            this.handleKeyboard(e);
        });
        
        // Auto-save state periodically
        setInterval(() => {
            if (this.state.screen === 'quiz') {
                this.saveState();
            }
        }, 5000);
        
        // Update timer every second during quiz
        setInterval(() => {
            if (this.state.screen === 'quiz' && this.startTime) {
                this.updateTimer();
            }
        }, 1000);
    }
    
    startQuiz() {
        this.state.screen = 'quiz';
        this.currentQuestionIndex = 0;
        this.score = 0;
        this.streak = 0;
        this.maxStreak = 0;
        this.startTime = Date.now();
        this.state.answeredQuestions = [];
        
        this.showScreen('quiz');
        this.displayQuestion();
        this.updateStats();
        this.saveState();
    }
    
    displayQuestion() {
        const question = this.questions[this.currentQuestionIndex];
        if (!question) return;
        
        // Update progress
        const progress = ((this.currentQuestionIndex + 1) / this.questions.length) * 100;
        document.getElementById('progress-fill').style.width = `${progress}%`;
        document.getElementById('question-counter').textContent = 
            `${this.currentQuestionIndex + 1} / ${this.questions.length}`;
        
        // Update difficulty indicator
        this.updateDifficultyIndicator(question.difficulty);
        
        // Display question
        document.getElementById('question-text').textContent = question.question;
        
        // Display options
        const optionsContainer = document.getElementById('options-container');
        optionsContainer.innerHTML = '';
        
        question.options.forEach((option, index) => {
            const optionElement = document.createElement('div');
            optionElement.className = 'option';
            optionElement.dataset.index = index;
            optionElement.textContent = option;
            optionElement.style.opacity = '1'; // Reset opacity for new question
            optionsContainer.appendChild(optionElement);
        });
        
        // Reset state
        this.selectedAnswer = null;
        const submitButton = document.getElementById('submit-answer');
        submitButton.disabled = true;
        submitButton.style.display = 'block'; // Show submit button for new question
        
        this.updateStats();
    }
    
    updateDifficultyIndicator(difficulty) {
        const levelMap = {
            'beginner': { text: 'Beginner', stars: 1 },
            'intermediate': { text: 'Intermediate', stars: 2 },
            'advanced': { text: 'Advanced', stars: 3 }
        };
        
        const level = levelMap[difficulty] || levelMap['beginner'];
        document.getElementById('difficulty-level').textContent = level.text;
        
        const starsContainer = document.getElementById('difficulty-stars');
        starsContainer.innerHTML = '';
        
        for (let i = 1; i <= 3; i++) {
            const star = document.createElement('span');
            star.className = i <= level.stars ? 'star' : 'star empty';
            star.textContent = '★';
            starsContainer.appendChild(star);
        }
    }
    
    selectOption(optionElement) {
        // Remove previous selection
        document.querySelectorAll('.option').forEach(opt => {
            opt.classList.remove('selected');
        });
        
        // Select new option
        optionElement.classList.add('selected');
        this.selectedAnswer = parseInt(optionElement.dataset.index);
        
        // Enable submit button
        document.getElementById('submit-answer').disabled = false;
    }
    
    submitAnswer() {
        if (this.selectedAnswer === null) return;
        
        const question = this.questions[this.currentQuestionIndex];
        const isCorrect = this.selectedAnswer === question.correct;
        
        // Update score and streak
        if (isCorrect) {
            this.score += this.calculatePoints(question.difficulty);
            this.streak++;
            this.maxStreak = Math.max(this.maxStreak, this.streak);
        } else {
            this.streak = 0;
        }
        
        // Store answer
        this.state.answeredQuestions.push({
            questionId: question.id,
            selectedAnswer: this.selectedAnswer,
            correct: isCorrect,
            timeSpent: Date.now() - this.questionStartTime || 0
        });
        
        // Mark question as answered
        this.state.questionAnswered = true;
        
        // Show feedback
        this.showFeedback(isCorrect, question);
        
        // Hide submit button and disable options
        document.getElementById('submit-answer').style.display = 'none';
        document.querySelectorAll('.option').forEach((opt, index) => {
            opt.style.pointerEvents = 'none';
            // Gray out options that weren't selected
            if (index !== this.selectedAnswer) {
                opt.style.opacity = '0.5';
            }
        });
        
        this.updateStats();
        this.saveState();
    }
    
    calculatePoints(difficulty) {
        const pointMap = {
            'beginner': 10,
            'intermediate': 20,
            'advanced': 30
        };
        
        let basePoints = pointMap[difficulty] || 10;
        
        // Bonus for streak
        if (this.streak >= 5) basePoints *= 1.5;
        else if (this.streak >= 3) basePoints *= 1.2;
        
        return Math.floor(basePoints);
    }
    
    showFeedback(isCorrect, question) {
        const feedbackSection = document.getElementById('feedback-section');
        const result = document.getElementById('feedback-result');
        const explanation = document.getElementById('feedback-explanation');
        const reference = document.getElementById('feedback-reference');
        const nextButton = document.getElementById('next-question');
        
        // Show result
        result.textContent = isCorrect ? '🎉 Correct!' : '❌ Incorrect';
        result.className = `feedback-result ${isCorrect ? 'correct' : 'incorrect'}`;
        
        // Show explanation
        explanation.textContent = question.explanation;
        
        // Show reference
        reference.innerHTML = `
            <strong>Reference:</strong> ${question.reference}<br>
            <a href="${question.referenceUrl}" target="_blank">📖 Read more in the manual</a>
        `;
        
        // Update button text for last question
        const isLastQuestion = this.currentQuestionIndex >= this.questions.length - 1;
        nextButton.textContent = isLastQuestion ? 'Finish Quiz' : 'Next Question';
        
        // Highlight correct answer
        document.querySelectorAll('.option').forEach((opt, index) => {
            if (index === question.correct) {
                opt.classList.add('correct');
            } else if (index === this.selectedAnswer && !isCorrect) {
                opt.classList.add('incorrect');
            }
        });
        
        // Show feedback section
        feedbackSection.classList.remove('hidden');
    }
    
    nextQuestion() {
        // Hide feedback section
        document.getElementById('feedback-section').classList.add('hidden');
        
        // Clear question answered flag
        this.state.questionAnswered = false;
        
        // Move to next question or show results
        this.currentQuestionIndex++;
        
        if (this.currentQuestionIndex >= this.questions.length) {
            this.showResults();
        } else {
            this.questionStartTime = Date.now();
            this.displayQuestion();
        }
    }
    
    showResults() {
        this.state.screen = 'results';
        this.state.totalTime = Date.now() - this.startTime;
        
        // Calculate accuracy based on answered questions vs total questions
        const correctAnswers = this.state.answeredQuestions.filter(q => q.correct).length;
        const totalQuestions = this.state.answeredQuestions.length;
        const accuracy = totalQuestions > 0 ? Math.round((correctAnswers / totalQuestions) * 100) : 0;
        
        // Update final stats
        document.getElementById('final-score').textContent = this.score;
        document.getElementById('final-accuracy').textContent = `${accuracy}%`;
        document.getElementById('final-time').textContent = this.formatTime(this.state.totalTime);
        document.getElementById('max-streak').textContent = this.maxStreak;
        
        // Update performance message
        this.updatePerformanceMessage(accuracy);
        
        this.showScreen('results');
        this.clearState(); // Clear saved progress
    }
    
    updatePerformanceMessage(accuracy) {
        const title = document.getElementById('performance-title');
        const description = document.getElementById('performance-description');
        
        if (accuracy >= 90) {
            title.textContent = 'Emacs Lisp Master! 🏆';
            description.textContent = 'Outstanding! You have truly mastered the art of Emacs Lisp. Your knowledge rivals that of the greatest Emacs wizards!';
        } else if (accuracy >= 80) {
            title.textContent = 'Emacs Lisp Expert! 🌟';
            description.textContent = 'Excellent work! You have a strong command of Emacs Lisp and are well on your way to mastery.';
        } else if (accuracy >= 70) {
            title.textContent = 'Emacs Lisp Practitioner! 👨‍💻';
            description.textContent = 'Good job! You have a solid understanding of Emacs Lisp fundamentals with room to grow.';
        } else if (accuracy >= 60) {
            title.textContent = 'Emacs Lisp Learner! 📚';
            description.textContent = 'You\'re on the right track! Keep studying and practicing to improve your Emacs Lisp skills.';
        } else {
            title.textContent = 'Keep Learning! 🌱';
            description.textContent = 'Don\'t give up! Emacs Lisp is a journey. Consider reviewing the official documentation and trying again.';
        }
    }
    
    shareResults() {
        const correctAnswers = this.state.answeredQuestions.filter(q => q.correct).length;
        const totalQuestions = this.state.answeredQuestions.length;
        const accuracy = totalQuestions > 0 ? Math.round((correctAnswers / totalQuestions) * 100) : 0;
        
        const shareData = {
            score: this.score,
            accuracy: accuracy,
            time: this.state.totalTime,
            maxStreak: this.maxStreak,
            questionsCount: totalQuestions,
            testMode: this.isTestMode
        };
        
        const encodedData = btoa(JSON.stringify(shareData));
        const shareUrl = `${window.location.origin}${window.location.pathname}?results=${encodedData}`;
        
        document.getElementById('share-link').value = shareUrl;
        document.getElementById('share-url').classList.remove('hidden');
    }
    
    copyShareLink() {
        const linkInput = document.getElementById('share-link');
        linkInput.select();
        linkInput.setSelectionRange(0, 99999); // For mobile devices
        
        try {
            document.execCommand('copy');
            const button = document.getElementById('copy-link');
            const originalText = button.textContent;
            button.textContent = 'Copied!';
            setTimeout(() => {
                button.textContent = originalText;
            }, 2000);
        } catch (err) {
            console.error('Failed to copy link:', err);
        }
    }
    
    restartQuiz() {
        this.clearState();
        location.reload();
    }
    
    displayQuestionWithFeedback() {
        const question = this.questions[this.currentQuestionIndex];
        if (!question) return;
        
        // Display the question normally first
        this.displayQuestion();
        
        // Find the answered question data
        const answeredQuestion = this.state.answeredQuestions.find(q => q.questionId === question.id);
        if (!answeredQuestion) return;
        
        // Restore the selected answer and show feedback
        this.selectedAnswer = answeredQuestion.selectedAnswer;
        
        // Hide submit button and gray out unselected options
        document.getElementById('submit-answer').style.display = 'none';
        document.querySelectorAll('.option').forEach((opt, index) => {
            opt.style.pointerEvents = 'none';
            if (index !== this.selectedAnswer) {
                opt.style.opacity = '0.5';
            }
            // Highlight correct/incorrect answers
            if (index === question.correct) {
                opt.classList.add('correct');
            } else if (index === this.selectedAnswer && !answeredQuestion.correct) {
                opt.classList.add('incorrect');
            }
        });
        
        // Show feedback
        this.showFeedback(answeredQuestion.correct, question);
    }
    
    updateStats() {
        document.getElementById('current-score').textContent = this.score;
        document.getElementById('current-streak').textContent = this.streak;
        this.updateTimer();
    }
    
    updateTimer() {
        if (this.startTime) {
            const elapsed = Date.now() - this.startTime;
            document.getElementById('current-time').textContent = this.formatTime(elapsed);
        }
    }
    
    formatTime(milliseconds) {
        const seconds = Math.floor(milliseconds / 1000);
        const minutes = Math.floor(seconds / 60);
        const remainingSeconds = seconds % 60;
        return `${minutes}:${remainingSeconds.toString().padStart(2, '0')}`;
    }
    
    handleKeyboard(e) {
        if (this.state.screen !== 'quiz') return;
        
        // Number keys for option selection
        if (e.key >= '1' && e.key <= '4') {
            const optionIndex = parseInt(e.key) - 1;
            const options = document.querySelectorAll('.option');
            if (options[optionIndex]) {
                this.selectOption(options[optionIndex]);
            }
        }
        
        // Enter to submit
        if (e.key === 'Enter' && this.selectedAnswer !== null) {
            if (!document.getElementById('feedback-section').classList.contains('hidden')) {
                this.nextQuestion();
            } else {
                this.submitAnswer();
            }
        }
        
        // Escape to close feedback
        if (e.key === 'Escape') {
            const feedbackSection = document.getElementById('feedback-section');
            if (!feedbackSection.classList.contains('hidden')) {
                this.nextQuestion();
            }
        }
    }
    
    showScreen(screenName) {
        // Hide all screens
        document.querySelectorAll('.screen').forEach(screen => {
            screen.classList.add('hidden');
        });
        
        // Show target screen
        document.getElementById(`${screenName}-screen`).classList.remove('hidden');
    }
    
    saveState() {
        const state = {
            screen: this.state.screen,
            currentQuestionIndex: this.currentQuestionIndex,
            score: this.score,
            streak: this.streak,
            maxStreak: this.maxStreak,
            startTime: this.startTime,
            answeredQuestions: this.state.answeredQuestions,
            isTestMode: this.isTestMode,
            questionAnswered: this.state.questionAnswered || false
        };
        
        localStorage.setItem('emacsQuizState', JSON.stringify(state));
    }
    
    loadState() {
        const saved = localStorage.getItem('emacsQuizState');
        if (!saved) return;
        
        try {
            const state = JSON.parse(saved);
            
            // Only restore if it's the same mode
            if (state.isTestMode !== this.isTestMode) {
                this.clearState();
                return;
            }
            
            // Only restore if quiz was in progress
            if (state.screen !== 'quiz') return;
            
            this.state.screen = state.screen;
            this.currentQuestionIndex = state.currentQuestionIndex;
            this.score = state.score;
            this.streak = state.streak;
            this.maxStreak = state.maxStreak;
            this.startTime = state.startTime;
            this.state.answeredQuestions = state.answeredQuestions || [];
            this.state.questionAnswered = state.questionAnswered || false;
            
            // Validate state
            if (this.currentQuestionIndex >= this.questions.length) {
                this.clearState();
                return;
            }
            
        } catch (err) {
            console.error('Failed to load saved state:', err);
            this.clearState();
        }
    }
    
    clearState() {
        localStorage.removeItem('emacsQuizState');
    }
    
    checkSharedResults() {
        const urlParams = new URLSearchParams(window.location.search);
        const resultsData = urlParams.get('results');
        
        if (resultsData) {
            try {
                const data = JSON.parse(atob(resultsData));
                this.displaySharedResults(data);
                return true; // Indicate that shared results were displayed
            } catch (err) {
                console.error('Failed to parse shared results:', err);
            }
        }
        return false;
    }
    
    displaySharedResults(data) {
        // Set up the results screen with shared data
        this.state.screen = 'results';
        this.score = data.score;
        this.maxStreak = data.maxStreak;
        this.state.totalTime = data.time;
        
        // Create mock answered questions for accuracy calculation
        const correctCount = Math.round((data.accuracy / 100) * data.questionsCount);
        this.state.answeredQuestions = Array.from({ length: data.questionsCount }, (_, i) => ({
            questionId: i + 1,
            correct: i < correctCount
        }));
        
        // Update final stats
        document.getElementById('final-score').textContent = this.score;
        document.getElementById('final-accuracy').textContent = `${data.accuracy}%`;
        document.getElementById('final-time').textContent = this.formatTime(this.state.totalTime);
        document.getElementById('max-streak').textContent = this.maxStreak;
        
        // Update performance message
        this.updatePerformanceMessage(data.accuracy);
        
        // Add a banner to indicate these are shared results
        const resultsContainer = document.querySelector('.results-container');
        const sharedBanner = document.createElement('div');
        sharedBanner.className = 'shared-banner';
        sharedBanner.style.cssText = `
            background: var(--gradient-2);
            color: white;
            padding: 1rem;
            border-radius: 10px;
            margin-bottom: 2rem;
            text-align: center;
            font-weight: 600;
        `;
        sharedBanner.innerHTML = `
            🎉 Shared Results from ${data.testMode ? 'Test Mode' : 'Full Quiz'}!<br>
            <small style="opacity: 0.9;">Can you beat this score? Take the quiz to find out!</small>
        `;
        resultsContainer.insertBefore(sharedBanner, resultsContainer.firstChild);
        
        // Hide share results button and update restart button text for shared results
        document.getElementById('share-results').style.display = 'none';
        document.getElementById('restart-quiz').textContent = 'Take Quiz';
        
        // Mark as shared results for restart handling
        this.isSharedResults = true;
        this.sharedTestMode = data.testMode;
        
        // Show results screen
        this.showScreen('results');
    }
}

// Initialize the quiz when the page loads
document.addEventListener('DOMContentLoaded', () => {
    const quiz = new EmacsLispQuiz();
    
    // Shared results are now checked in init(), no need for delayed check
    
    // Add some CSS animations dynamically
    const style = document.createElement('style');
    style.textContent = `
        @keyframes slideIn {
            from { transform: translateX(100%); opacity: 0; }
            to { transform: translateX(0); opacity: 1; }
        }
        
        .option {
            transition: all 0.3s ease;
        }
        
        .option:hover {
            transform: translateX(5px);
        }
        
        .progress-fill {
            transition: width 0.8s cubic-bezier(0.4, 0, 0.2, 1);
        }
        
        .feedback-section {
            animation: slideDown 0.3s ease;
        }
        
        .stat-value {
            transition: all 0.3s ease;
        }
        
        .btn {
            position: relative;
            overflow: hidden;
        }
        
        .btn::before {
            content: '';
            position: absolute;
            top: 50%;
            left: 50%;
            width: 0;
            height: 0;
            background: rgba(255, 255, 255, 0.1);
            border-radius: 50%;
            transform: translate(-50%, -50%);
            transition: width 0.6s, height 0.6s;
        }
        
        .btn:active::before {
            width: 300px;
            height: 300px;
        }
    `;
    document.head.appendChild(style);
});
