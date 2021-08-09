/** Standard event bus
 *
 * Expected events:
 * * error -- Show notification in UI; error details are event value.
 *
 * */
import Vue from 'vue';

const bus = new Vue;
/** Convenience method for async catch() */
(bus as any).error = (e: any) => {
  bus.$emit('error', e);
  throw e;
};

interface BusType {
  error: (e: any) => void;
  $emit: (name: string, e: any) => void;
  $off: (name: string, cb?: (e: any) => void) => void;
  $on: (name: string, cb: (e: any) => void) => void;
}

export default bus as BusType;

