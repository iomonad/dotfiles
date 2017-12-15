pipeline {
  agent any
  stages {
    stage('Fix sensitive data') {
      steps {
        sh 'find . -name "*.db" -print'
      }
    }
  }
}