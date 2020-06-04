<template>
  <div id="app">
    <Home />
  </div>
</template>

<script lang="ts">
import Vue from 'vue';
import Home from './components/Home.vue';

export default Vue.extend({
  name: 'App',
  components: {
    Home,
  }
});
</script>

<style lang="scss">
body {
  margin: 0;
  padding: 0;
}

#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 0px;
}
</style>
