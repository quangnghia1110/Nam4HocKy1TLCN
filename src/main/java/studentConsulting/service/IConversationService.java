package studentConsulting.service;

import java.util.List;
import java.util.Optional;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.ConversationDTO;
import studentConsulting.model.payload.request.socket.ConversationRequest;

public interface IConversationService {
    public ConversationDTO createConversation(ConversationRequest request, UserInformationEntity user);
    public List<ConversationDTO> findConversationsByUserId(Integer userId);
    public ConversationDTO findConversationById(Integer conversationId);
}
