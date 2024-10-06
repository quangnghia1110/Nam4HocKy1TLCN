package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import studentConsulting.model.payload.dto.question_answer.AnswerDTO;
import studentConsulting.model.payload.request.question_answer.ReviewAnswerRequest;
import studentConsulting.model.payload.request.question_answer.UpdateAnswerRequest;

import java.time.LocalDate;
import java.util.Optional;

public interface IAdvisorAnswerService {

    AnswerDTO reviewAnswer(ReviewAnswerRequest request);

    Page<AnswerDTO> getApprovedAnswersByDepartmentWithFilters(Optional<Integer> departmentId, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);

    Page<AnswerDTO> getAllAnswersByDepartmentWithFilters(Optional<Integer> departmentId, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);

    AnswerDTO updateAnswer(Integer answerId, UpdateAnswerRequest request);

    AnswerDTO updateAnswerByDepartment(Integer answerId, UpdateAnswerRequest request, Integer departmentId);

    void deleteAnswer(Integer id);

    void deleteAnswerByDepartment(Integer id, Integer departmentId);

    AnswerDTO getAnswerById(Integer answerId);

    AnswerDTO getAnswerByIdAndDepartment(Integer answerId, Integer departmentId);
}
