package studentConsulting.controller.consultant;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.model.payload.request.question_answer.ForwardQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.consultant.IConsultantQuestionService;

import java.security.Principal;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class ConsultantForwardQuestionController {

    @Autowired
    private IConsultantQuestionService questionService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PostMapping("/consultant/forward-question/forward")
    public DataResponse<ForwardQuestionDTO> forwardQuestion(@RequestBody ForwardQuestionRequest forwardQuestionRequest,
                                                            Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new Exceptions.ErrorException("Không tìm thấy người dùng");
        }

        return questionService.forwardQuestion(forwardQuestionRequest, email);
    }
}
