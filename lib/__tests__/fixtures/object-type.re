type subscribeOptions = {. "start": float, "stop": float};

type testOptions = {. "method": string, "subscribe": subscribeOptions};

[@bs.module "object-type"] external test : (~options: testOptions) => string = "";