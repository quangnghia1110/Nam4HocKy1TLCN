package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.ConversationEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.ConversationDTO;
import studentConsulting.model.payload.dto.actor.EmailDTO;
import studentConsulting.model.payload.dto.actor.MemberDTO;
import studentConsulting.model.payload.request.CreateConversationRequest;
import studentConsulting.model.payload.request.CreateConversationUserRequest;

import java.time.LocalDate;
import java.util.List;

public interface IConversationService {
    ConversationDTO createConversation(CreateConversationUserRequest request, UserInformationEntity user);

    Page<ConversationDTO> getConversationByRole(Integer userId, String role, Integer depId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ConversationDTO getDetailConversationByRole(Integer conversationId);

    ConversationDTO createConversationByConsultant(CreateConversationRequest request, UserInformationEntity user);

    ConversationDTO approveMembersByEmail(Integer groupId, List<String> emailsToApprove);

    void deleteConversation(Integer conversationId);

    public boolean recordDeletion(Integer conversationId, Integer userId);

        void updateConversationName(Integer conversationId, String newName);

    void removeMemberFromConversation(Integer conversationId, Integer userId);

    List<MemberDTO> findNonConsultantMembers(Integer conversationId);

    List<EmailDTO> findAllUsers();
    public void deleteMembersFromConversation(ConversationEntity conversation, Integer userId);

    }
