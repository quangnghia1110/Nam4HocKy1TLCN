package studentConsulting.constant;

import java.util.Optional;

import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.repository.UserRepository;

@Service
public class SecurityService {

    
    public Optional<UserInformationEntity> getAuthenticatedUser(String username, UserRepository userRepository) {
        return userRepository.findByAccountUsername(username);
    }
    
    
}

