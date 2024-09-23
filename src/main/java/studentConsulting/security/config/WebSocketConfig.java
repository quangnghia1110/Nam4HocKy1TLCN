package studentConsulting.security.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.simp.config.MessageBrokerRegistry;
import org.springframework.web.socket.config.annotation.EnableWebSocketMessageBroker;
import org.springframework.web.socket.config.annotation.StompEndpointRegistry;
import org.springframework.web.socket.config.annotation.WebSocketMessageBrokerConfigurer;

@Configuration
@EnableWebSocketMessageBroker
public class WebSocketConfig implements WebSocketMessageBrokerConfigurer {
    @Override
    public void registerStompEndpoints(StompEndpointRegistry registry) {
        registry.addEndpoint("/ws").setAllowedOriginPatterns("*").withSockJS();
    }

    @Override
    public void configureMessageBroker(MessageBrokerRegistry registry) {
        //Từ client đến server
    	registry.setApplicationDestinationPrefixes("/app");
        //topic là cho nhóm chung (chưa xử lý)
    	//queue và user dành cho nhóm riêng
        registry.enableSimpleBroker("/chatroom","/user");
        //Từ server đến client
        registry.setUserDestinationPrefix("/user");
    }
}


