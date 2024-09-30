package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.communication.ConversationDTO;

import java.time.LocalDate;

public interface IAdvisorConversationService {
    void deleteConversation(Integer conversationId, Integer departmentId);

    void updateConversationName(Integer conversationId, String newName, Integer departmentId);

    Page<ConversationDTO> findConversationsByDepartmentWithFilters(Integer departmentId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable);

    void removeMemberFromConversation(Integer conversationId, Integer userId, Integer departmentId);

}
