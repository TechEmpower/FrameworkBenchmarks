export const generateRandomNumber = () => Math.floor(Math.random() * 10000) + 1;

export const parseQuery = (i: string | string[] | undefined) =>
  Math.min(Number(i) || 1, 500);
