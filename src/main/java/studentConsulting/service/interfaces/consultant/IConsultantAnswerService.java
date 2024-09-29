package studentConsulting.service.interfaces.consultant;

import studentConsulting.model.payload.dto.question_answer.AnswerDTO;
import studentConsulting.model.payload.request.question_answer.CreateAnswerRequest;

public interface IConsultantAnswerService {
    AnswerDTO createAnswer(CreateAnswerRequest request);
}
