package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import studentConsulting.model.payload.dto.question_answer.AnswerDTO;
import studentConsulting.model.payload.request.question_answer.UpdateAnswerRequest;

import java.time.LocalDate;

public interface IAdminAnswerService {

    Page<AnswerDTO> getApprovedAnswersByDepartmentWithFilters(LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);

    Page<AnswerDTO> getAllAnswersByDepartmentWithFilters(LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);

    AnswerDTO updateAnswer(Integer answerId, UpdateAnswerRequest request);

    void deleteAnswer(Integer id);

    AnswerDTO getAnswerById(Integer answerId);
}
