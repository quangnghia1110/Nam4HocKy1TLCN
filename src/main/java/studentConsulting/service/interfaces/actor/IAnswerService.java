package studentConsulting.service.interfaces.actor;

import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.AnswerDTO;
import studentConsulting.model.payload.request.CreateAnswerRequest;
import studentConsulting.model.payload.request.ReviewAnswerRequest;
import studentConsulting.model.payload.request.UpdateAnswerRequest;

public interface IAnswerService {
    AnswerDTO createAnswer(CreateAnswerRequest request);

    AnswerDTO reviewAnswer(ReviewAnswerRequest request);

    AnswerDTO updateAnswer(Integer answerId, UpdateAnswerRequest request, UserInformationEntity user);

    void deleteAnswer(Integer id, UserInformationEntity user);

    AnswerDTO getAnswerById(Integer answerId, UserInformationEntity user);
}
