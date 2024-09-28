package studentConsulting.service.interfaces.consultant;

import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;

public interface IConsultantAnswerService {
    AnswerDTO createAnswer(CreateAnswerRequest request);
}
