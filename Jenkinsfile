pipeline {
  agent any
  stages {
    stage('Fix sensitive data') {
      steps {
        sh 'find . -name "*.db" -print'
        sh 'find . -name "*.history" -print'
        sh 'find . -name "*dump" -print'
      }
    }
  }
}
