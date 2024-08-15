import { Providers, Class } from '@ditsmod/core';

export class BunProviders extends Providers {
  protected setCondition?: boolean;
  protected ifCondition?: boolean;

  if(condition: any) {
    this.setCondition = true;
    this.ifCondition = condition;
    return this;
  }

  override useClass<A extends Class, B extends A>(token: A, useClass: B, multi?: boolean): this {
    if (!this.setCondition || this.ifCondition) {
      this.pushProvider({ token, useClass }, multi);
    }
    this.setCondition = undefined;
    this.ifCondition = undefined;
    return this;
  }
}
