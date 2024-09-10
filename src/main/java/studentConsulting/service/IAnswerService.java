package studentConsulting.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;

public interface IAnswerService {
    public AnswerDTO createAnswer(CreateAnswerRequest request);
	public AnswerDTO reviewAnswer(CreateAnswerRequest request);
}
