package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.ReviewAnswerRequest;
import studentConsulting.model.payload.request.answer.UpdateAnswerRequest;

import java.time.LocalDate;

public interface IAdvisorAnswerService {
   
    AnswerDTO reviewAnswer(ReviewAnswerRequest request);

    Page<AnswerDTO> getApprovedAnswersByDepartmentWithFilters(Integer departmentId, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);

    Page<AnswerDTO> getAllAnswersByDepartmentWithFilters(Integer departmentId, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);

    AnswerDTO updateAnswer(Integer answerId, UpdateAnswerRequest request);

    void deleteAnswer(Integer id, Integer departmentId);
}
