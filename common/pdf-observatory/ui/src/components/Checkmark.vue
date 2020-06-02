<template lang="pug">
  span.checkmark
    v-icon(v-if="status === 'valid'" color="green") mdi-check-outline
    v-icon(v-else-if="status === 'rejected'" color="red") mdi-do-not-disturb
    v-icon(v-else-if="status === 'ignore'" color="gray") mdi-close-outline
    span(v-else) &lt;no icon for {{status}}&gt;
</template>

<script lang="ts">
import Vue from 'vue';

export default Vue.extend({
  props: {
    status: String as () => 'valid' | 'rejected' | 'ignore',
  },
});
</script>

