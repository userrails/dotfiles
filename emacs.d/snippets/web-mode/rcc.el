# -*- mode: snippet -*-
# name: React.createClass(...)
# key: rcc
# --
import React, { Component } from 'react';

class ${1:App} extends Component {
  render() {
    return (
      $0
    );
  }
}

export default $1;