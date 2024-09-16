package studentConsulting.service;

import java.util.List;
import java.util.Optional;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.ConversationDTO;
import studentConsulting.model.payload.dto.MemberDTO;
import studentConsulting.model.payload.request.socket.CreateConversationRequest;

public interface IConversationService {
    ConversationDTO createConversation(CreateConversationRequest request, UserInformationEntity user);
    List<ConversationDTO> findConversationsByUserId(Integer userId);
    public List<ConversationDTO> findConversationsByConsultantId(Integer consultantId);
    ConversationDTO findConversationById(Integer conversationId);
    public ConversationDTO approveMember(Integer groupId, Integer userId);
    public ConversationDTO createConversationByConsultant(CreateConversationRequest request, UserInformationEntity user);
    public void deleteConversation(Integer conversationId);
    
    public void updateConversationName(Integer conversationId, String newName);
    
    public void removeMemberFromConversation(Integer conversationId, Integer userId);
    
    public List<MemberDTO> findNonConsultantMembers(Integer conversationId);

}
