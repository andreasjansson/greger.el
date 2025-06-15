# Thinking Tests for greger.el

This document describes the tests for the thinking functionality in greger.el.

## Test Structure

The thinking functionality is tested at multiple levels:

### Unit Tests (`test-greger-client.el`)

#### `greger-client-test-thinking-configuration`
- **Purpose**: Tests that thinking configuration is properly added to API requests
- **Tags**: `unit`, `thinking`
- **Coverage**:
  - Thinking enabled with custom budget (2048 tokens)
  - Thinking disabled (budget = 0)
  - No thinking parameter (budget = nil)
  - Verifies correct JSON structure in requests
  - Verifies max_tokens calculation includes thinking budget

### End-to-End Tests (`test-end-to-end.el`)

#### `greger-end-to-end-test-thinking-functionality`
- **Purpose**: Tests thinking functionality works end-to-end with real API calls
- **Tags**: `end-to-end`, `thinking`, `api`
- **Requirements**: Valid `ANTHROPIC_API_KEY` environment variable
- **Coverage**:
  - Creates greger buffer with thinking-enabled prompt
  - Verifies thinking content appears correctly in buffer
  - Validates thinking content comes before assistant response
  - Handles cases where Claude chooses not to use thinking

#### `greger-end-to-end-test-thinking-toggle`
- **Purpose**: Tests the thinking toggle functionality
- **Tags**: `end-to-end`, `thinking`
- **Coverage**:
  - Toggle from disabled to enabled (default 4096 tokens)
  - Toggle from enabled back to disabled
  - Verifies state changes correctly

#### `greger-end-to-end-test-thinking-mode-line`
- **Purpose**: Tests thinking status display in mode line
- **Tags**: `end-to-end`, `thinking`, `ui`
- **Coverage**:
  - Mode line shows `[T:2048]` when thinking enabled
  - Mode line hides thinking info when disabled
  - Verifies UI feedback works correctly

## Running the Tests

### All Thinking Tests
```bash
cd test
emacs --script run-thinking-tests.el
```

### Individual Test Categories
```bash
# Unit tests only
emacs --batch --eval "(progn (add-to-list 'load-path \".\") (require 'ert) (load \"test/test-greger-client.el\") (ert-run-tests-batch-and-exit '(tag thinking)))"

# End-to-end tests (no API calls)
emacs --batch --eval "(progn (add-to-list 'load-path \".\") (require 'ert) (load \"test/test-end-to-end.el\") (ert-run-tests-batch-and-exit '(and (tag thinking) (not (tag api)))))"

# API tests (requires ANTHROPIC_API_KEY)
emacs --batch --eval "(progn (add-to-list 'load-path \".\") (require 'ert) (load \"test/test-end-to-end.el\") (ert-run-tests-batch-and-exit '(and (tag thinking) (tag api))))"
```

### Specific Tests
```bash
# Run individual test
emacs --batch --eval "(progn (add-to-list 'load-path \".\") (require 'ert) (load \"test/test-greger-client.el\") (ert-run-tests-batch-and-exit 'greger-client-test-thinking-configuration))"
```

## Test Coverage

The tests cover the complete thinking implementation:

1. **Configuration**: Thinking budget settings and validation
2. **API Integration**: Request building with thinking parameters
3. **Response Handling**: Processing thinking content in streaming responses
4. **UI Integration**: Mode line display and toggle functionality
5. **End-to-End Workflow**: Complete user experience with thinking

## Test Dependencies

- **Unit Tests**: No external dependencies
- **End-to-End Tests (non-API)**: No external dependencies
- **API Tests**: Require valid `ANTHROPIC_API_KEY` environment variable

## Expected Behavior

### When Thinking is Enabled
- API requests include `{"thinking": {"type": "enabled", "budget_tokens": N}}`
- Max tokens = base_tokens + thinking_budget
- Thinking content appears under `# THINKING` headers
- Mode line shows `[T:N]` where N is the token budget

### When Thinking is Disabled
- API requests include `{"thinking": {"type": "disabled"}}`
- Max tokens = base_tokens only
- No thinking headers appear
- Mode line shows no thinking indicator

### When Thinking is Undefined
- API requests omit thinking parameter entirely
- API uses default thinking behavior
- Max tokens = base_tokens only
