# app.py

from flask import Flask, render_template, request, jsonify
from expr2latex import expr2latex

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/convert', methods=['POST'])
def convert():
    expr = request.json.get('expression', '')
    try:
        latex_output = expr2latex(expr)
        return jsonify({'success': True, 'latex': latex_output})
    except Exception as e:
        app.logger.error(f"Error converting expression: {e}")
        return jsonify({'success': False, 'error': 'Invalid expression. Please check your input.'})

if __name__ == "__main__":
    app.run(debug=True)