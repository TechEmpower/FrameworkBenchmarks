let date = new Date().toUTCString();
setInterval(() => (date = new Date().toUTCString()), 850);
export const getDate = () => date;