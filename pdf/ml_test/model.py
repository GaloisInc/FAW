
import pyobjconfig.torch
import torch

class ActionSet(pyobjconfig.ConfigurableObject):
    class config(pyobjconfig.PydanticBaseModel):
        anchor_reward: float = 1
        subgrammar_reward: float = 1

class Frequencies(pyobjconfig.ConfigurableObject):
    class config(pyobjconfig.PydanticBaseModel):
        tokens_per_decay: float = 1

class Model(pyobjconfig.torch.ConfigurableModule):
    """This is a stub model for the real RL-GRIT model, which isn't cleaned up
    enough yet to go in the public repo.
    """
    class config(pyobjconfig.PydanticBaseModel):
        net_context: int = 0

    action_set = ActionSet
    frequencies = Frequencies

    def build(self):
        """Build this model.
        """
        self.embeddings = torch.nn.Embedding(256, 32)
        self._optim = torch.optim.Adam(self.parameters())


    def parse(self, sents):
        return [0. for s in sents], {}


    def train_batch(self, sents):
        loss = 0.
        loss_count = 0
        embs = [self.embeddings(torch.tensor([ord(c) for c in s])) for s in sents]
        for s in embs:
            for a, b in zip(s[:-1], s[1:]):
                loss_count += 1
                loss += (a - b).pow(2).sum() + (1 - a.pow(2).sum()).pow(2)
        (loss / loss_count).backward()
        self._optim.step()
        self._optim.zero_grad()

