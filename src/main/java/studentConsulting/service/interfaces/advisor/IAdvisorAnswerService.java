package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.question_answer.AnswerDTO;
import studentConsulting.model.payload.request.question_answer.ReviewAnswerRequest;
import studentConsulting.model.payload.request.question_answer.UpdateAnswerRequest;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface IAdvisorAnswerService {

    AnswerDTO reviewAnswer(ReviewAnswerRequest request);

    Page<AnswerDTO> getAllAnswersByDepartmentWithFilters(Optional<Integer> departmentId, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);

    AnswerDTO updateAnswer(Integer answerId, UpdateAnswerRequest request, UserInformationEntity user);

    void deleteAnswer(Integer id, UserInformationEntity user);

    AnswerDTO getAnswerById(Integer answerId, UserInformationEntity user);

    void importAnswers(List<List<String>> csvData);

}
