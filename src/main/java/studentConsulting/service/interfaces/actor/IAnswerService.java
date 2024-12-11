package studentConsulting.service.interfaces.actor;

import org.springframework.http.ResponseEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.AnswerDTO;
import studentConsulting.model.payload.request.CreateAnswerRequest;
import studentConsulting.model.payload.request.ReviewAnswerRequest;
import studentConsulting.model.payload.request.UpdateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;

public interface IAnswerService {
    public ResponseEntity<DataResponse<AnswerDTO>> createAnswer(CreateAnswerRequest request);

    public AnswerDTO reviewAnswer(Integer questionId, ReviewAnswerRequest request);

    AnswerDTO updateAnswer(Integer answerId, UpdateAnswerRequest request, UserInformationEntity user);

    void deleteAnswer(Integer id, UserInformationEntity user);

    AnswerDTO getAnswerById(Integer answerId, UserInformationEntity user);
}
