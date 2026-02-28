// SSE subscriber with exponential backoff reconnect.
// Shared implementation for both simple and typed SSE subscriptions.
const http = require('http');

function createSseSubscriber(url, onMessage) {
  let req;
  let buffer = '';
  let currentEvent = 'message';
  let retryDelay = 1000;
  const maxDelay = 30000;

  const reconnect = () => {
    retryDelay = Math.min(retryDelay * 2, maxDelay);
    setTimeout(startListening, retryDelay);
  };

  const startListening = () => {
    req = http.get(url, { timeout: 0 }, (res) => {
      retryDelay = 1000;
      res.on('data', (chunk) => {
        buffer += chunk.toString();
        const lines = buffer.split('\n');
        buffer = lines.pop() || '';
        for (const line of lines) {
          if (line.startsWith('event: ')) {
            currentEvent = line.slice(7).trim();
          } else if (line.startsWith('data: ')) {
            try {
              const data = JSON.parse(line.slice(6));
              onMessage(currentEvent, data);
            } catch (e) { console.warn('[SageFs SSE] JSON parse error:', e.message); }
            currentEvent = 'message';
          } else if (line.trim() === '') {
            currentEvent = 'message';
          }
        }
      });
      res.on('end', reconnect);
      res.on('error', reconnect);
    });
    req.on('error', reconnect);
  };

  startListening();
  return { dispose: () => { if (req) req.destroy(); } };
}

module.exports = { createSseSubscriber };
