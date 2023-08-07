import { vitePreprocess } from '@sveltejs/kit/vite';
const { RUNTIME } = process.env;

let adapter;
if (RUNTIME === 'bun') {
	adapter = await import('svelte-adapter-bun');
} else {
	adapter = await import('@sveltejs/adapter-node');
}

/** @type {import('@sveltejs/kit').Config} */
const config = {
	// Consult https://kit.svelte.dev/docs/integrations#preprocessors
	// for more information about preprocessors
	preprocess: vitePreprocess(),

	kit: {
		// adapter-auto only supports some environments, see https://kit.svelte.dev/docs/adapter-auto for a list.
		// If your environment is not supported or you settled on a specific environment, switch out the adapter.
		// See https://kit.svelte.dev/docs/adapters for more information about adapters.
		adapter: adapter.default()
	}
};

export default config;
