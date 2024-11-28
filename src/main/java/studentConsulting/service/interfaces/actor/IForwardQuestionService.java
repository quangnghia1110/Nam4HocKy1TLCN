package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.actor.ForwardQuestionDTO;
import studentConsulting.model.payload.request.ForwardQuestionRequest;
import studentConsulting.model.payload.request.UpdateForwardQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;

import java.time.LocalDate;

public interface IForwardQuestionService {
    DataResponse<ForwardQuestionDTO> forwardQuestion(ForwardQuestionRequest forwardQuestionRequest, String username);

    Page<ForwardQuestionDTO> getForwardQuestionByRole(String title, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor);

    ForwardQuestionDTO updateForwardQuestionByRole(Integer forwardQuestionId, UpdateForwardQuestionRequest forwardQuestionRequest, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor);

    void deleteForwardQuestionByRole(Integer forwardQuestionId, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor);

    ForwardQuestionDTO getForwardQuestionDetailByRole(Integer forwardQuestionId, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor);
}
